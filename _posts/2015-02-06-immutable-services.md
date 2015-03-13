---
layout: post
title: Building services on top of immutable data structures
---
Using immutable data structures enables equational reasoning and assures that update operations are atomic. However, purely immutable interfaces are not always feasible. For instance a *RESTful* service typically needs to propagate the effects of update operations to other clients. In this post I describe a strategy for constructing mutable service interfaces on top of purely immutable data structures. I'm using F# to exemplify.


## Building a Player Service
Consider the task of designing a service for managing player information for some online game. The core API needs to support methods for retrieving and updating a set of players given some search criterion; Here is simple interface capturing the requirements:

{% highlight fsharp %}
type PlayerService = {
    FindPlayers : SearchConfiguration -> list<Player>
    UpdatePlayers : SearchConfiguration -> (Player -> Player) -> unit
}
{% endhighlight %}

More specific operations such as adding or removing player credits can be implemented in terms of `UpdatePlayers` and potentially exposed via a web-service API.

Also assume the following set of constraints:

1. All invocations of `UpdatePlayers` are atomic and executed in sequence.
2. Invocations of `UpdatePlayers` do not block `FindPlayers`.
3. `FindPlayers` is non-blocking.
4. `FindPlayers` always returns data from a consistent state.

(1) is crucial in order to make sure that effects of update operations are predictable. If one update operation adds 100 credits to a player and another one removes 40 from the same player, the net result should be plus 60 credits once both operations have been completed. (2) assures that consumers will still be able to read data from the service without waiting for potentially slow update operations or any other `FindPlayer` invocations to complete. (4) means that data from an incomplete update should never be accessible. For example, transferring credits from one player to another within one update operation must not allow a service consumer to observe an intermediate state where the total number of credits is any different from before and after the request was handled. Note that it is not required that update operations have an immediate effect on subsequent `FindPlayers` requests, this is sometimes referred to as *eventual consistency*.

The above interface is apparently *non-pure* since `UpdatePlayers` impacts future behavior of `FindPlayers` as a side effect.

What is a good data-structure for this problem? It may seem intuitive to pick a traditional mutable dictionary (such a a `HashTable`) for maintaining the set of players when realizing the interface. However, there are several challenges with such approach; Assuring atomicity of update operations would require a locking strategy, but blocking `FindPlayers` while waiting for the lock to be released is not feasible because of constraint (2). Ignoring the lock (*dirty reading*) is also not an option since it may violate (4) by leaking data from an inconsistent state.


## An immutable version
Rather than addressing these problems in a mutable setting, let's start by designing a pure interface with the hope of later being able to convert it to a mutable version:

{% highlight fsharp %}
type PlayerServicePure = 
    {
        FindPlayers : SearchConfiguration -> list<Player>
        UpdatePlayers : SearchConfiguration -> (Player -> Player) -> PlayerServicPure
    }
{% endhighlight %}

The only difference from the previous API is that `UpdatePlayers` now returns the next state of the service. 

The following example uses a standard FSharp *Map* for storing players. Simplistic definitions of `SearchConfiguration` and `Player` are also provided:

{% highlight fsharp %}
// Represent a player.
type Player = {Name : string ; Credit : int }

// Search configuration is simply a list of names.
type SearchConfiguration = { Names : list<string> }

// This is the pure interface for a player-service.
type PlayerServicePure = {
    FindPlayers : SearchConfiguration -> list<Player>
    UpdatePlayers : SearchConfiguration -> (Player -> Player) ->PlayerServicePure 
}

// Builds a service object given a set of players.
let buildPlayerServicePure players =
    let rec build playerMap =        
        {
             FindPlayers = fun sc -> 
                sc.Names
                |> List.choose (fun name -> Map.tryFind name playerMap)
             UpdatePlayers = fun sc f ->
                (playerMap, sc.Names)
                ||> Seq.fold (fun pMap name ->
                   match Map.tryFind name pMap with
                   | Some p -> Map.add name (f p) pMap
                   | None   -> pMap 
                )
                |> build
        }
    players 
    |> List.map (fun name -> (name, {Name = name; Credit = 0}))
    |> Map.ofSeq
    |> build     
{% endhighlight %}

Given a sequence of players, `buildPlayersService` constructs a service object. Here are some examples of how to program with the immutable service:

{% highlight fsharp %}
// Build a new service
let service = buildPlayerServicePure ["John"; "Jane"; "James"]

// Find all John players
let johns = service.FindPlayers ({Names = ["John"]})

// New service with richer Johns
let service = service.UpdatePlayers {Names = ["John"]} <| fun p -> 
    {p with Credit = p.Credit + 100}

{% endhighlight %}

Update operations are atomic by definition since the only way of noticing the effect of an update is via the next service state returned by `UpdatePlayers`. Any other problems requiring locking or synchronization do not apply.

Designing and reasoning about the pure implementation is straight forward but we are not able to directly build a web-service interface on top of it. The fundamental problem is that any update operation requested by one user would not be visible to others users.


## Deriving a service wrapper
To implement the original interface we wish to define a transformation from values of `PlayerServicePure` to `PlayerService` values also satisfying the constraints given above. The strategy is to capture the latest state of the service by a mutable reference and replace it with newer versions as update operations are processed. Here is an initial attempt:

{% highlight fsharp %}
let buildService players =
    let service = ref buildPlayerServiceIM players
    {
        FindPlayers = fun sc -> service.Value.findPlayers sc
        UpdatePlayers = fun sc ->
            service := service.Value.UpdatePlayers sc
    }
{% endhighlight %}

`FindPlayers` always returns the values of the current service object. The problem with this implementation is the definition of `UpdatePlayers`. If a second invocation of the function is performed before the first update operation terminates, the updates will be applied to the same service object and the first one to terminate will be discarded. This violates constraint (1).

To accommodate for this we must synchronize the updates so that subsequent operations will be placed in a queue in case the service is currently updating.
There is already built in support for this in F# via the `MailBoxProccing` library providing message passing capabilities between concurrent computations.Here is the extended version based on a `MailBox` process:

{% highlight fsharp %}
let buildPlayerService players =
    let service = ref <| buildPlayerServicePure players
    let updateProc = MailboxProcessor.Start <| fun inbox ->
        let rec proc () =
            async {
                let! (sc, f) = inbox.Receive ()
                service := service.Value.UpdatePlayers sc f
                return! proc ()
            }
        proc ()
    {
        FindPlayers = fun sc ->
            service.Value.FindPlayers sc
        UpdatePlayers = fun sc f ->
            updateProc.Post (sc,f)
    }
{% endhighlight %}

Incoming update requests are now processed one at the time since the recursive call to loop is performed first after an update operation is completed. This addresses constraint number (1). All updates are processed within an asynchronous computation which ensures that invocations of `FindPlayers` are not blocked, satisfying constraint (2). FindPlayers is non-blocking in accordance with (3). Thanks to the immutable underlying structure we are also guaranteed to always fetch player data from a consistent state as required by constraint (4).

Following is an example of programming with the interface:

{% highlight fsharp %}
// Build the service
let service =
    buildPlayerService [ "John" ; "Jane" ; "James" ] 
    
// Extracts credit from player with the given name.
let checkCredit name =
    match service.FindPlayers {Names = [name]} with
    | [p]   -> p.Credit
    | _     -> failwith "Player not found"

// Slowly adds some credit to a player.
let addCredit name amount =
    service.UpdatePlayers {Names = [name]} <| fun player -> 
        System.Threading.Thread.Sleep 200
        {player with Credit = player.Credit + amount}

// Runs 10 async computations each adding 1 credit to John 100 times.
// Total number of credits once completed should be 1000.
List.init 10 (fun _ -> 
    async {
        List.replicate 100 1
        |> List.iter (addCredit "John")
        return ()
    }
)
|> Async.Parallel
|> Async.Ignore
|> Async.Start

// Repeatedly prints the current credit of John
let rec trackJohn () =
    async {
        printfn "Current credit for John %A" <| checkCredit "John"
        do! Async.Sleep 1000
        return! trackJohn ()
    }
trackJohn ()
|> Async.Start
{% endhighlight %}

The `addCredit` function is deliberately made slower in order to test that the implementation can handle queued update operations. Ten *async computations* each invoking `addCredit` 100 times, increasing the amount of credit for the player *John*. In the meanwhile another thread repeatedly reports the current credit status. Here is some output of running the program:

{% highlight fsharp %}
Current credit for John is 48
Current credit for John is 95
...
Current credit for John is 918
Current credit for John is 967
Current credit for John is 1000
Current credit for John is 1000
Current credit for John is 1000
{% endhighlight %}

As seen, all update operations were accounted for. That's not a guarantee but at least an indication that the implementation is sound.

By introducing a thin service layer on top of a pure immutable interface, much of the headache typically involved in designing thread-safe mutable data structures can be avoided. Using this strategy, business logic may be expressed solely in terms of pure functions. Although this example is may seem specific, my experience is that the pattern is general and applicable on many types or services providing CRUD interfaces.


