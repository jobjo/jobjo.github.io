---
layout: post
title: The mice moving challenge
---

A few weeks ago I came across a logic puzzle handed out as a *holiday challenge*. 
I didn't solve it by hand but instead turned to *Haskell* for some help. As it proved to be a fun 
exercise I decided to pass it on and invited some friends to 
contribute with solutions in a language of their choice. I here present
the given puzzle along with the set of submissions received.

## The problem
There are two types of mice, left leaning (`<`) and right leaning (`>`).
Six mice are initially arranged on seven slots in the following configuration:

{% highlight haskell %}
[>,>,>,_,<,<,<]
{% endhighlight %}

The empty slot is indicated by `_` and the task is to move all 
mice on the left to the right and all mice on the right to the
left, as in:

{% highlight haskell %}
[<,<,<,_,>,>,>]
{% endhighlight %}

The rules are simple:

1. A right leaning mouse (`>`) can only move to the right.
2. A left leaning mouse (`<`) can only move to the left.  
3. A mouse can either move one step forward (left/right) to fill an empty slot 
or jump over any other mouse (left or right leaning) to occupy an empty slot.  

From the initial configuration:

{% highlight haskell %}
[>,>,>,_,<,<,<]
{% endhighlight %}

the following set of moves are therefore allowed:

{% highlight haskell %}
[>,>,_,>,<,<,<] - Move right leaning mouse one step forward.
[>,>,>,<,_,<,<] - Move left leaning mouse one step forward.
[>,_,>,>,<,<,<] - Jump over one mouse from the left.
[>,>,>,<,<,_,<] - Jump over one mouse from the right.
{% endhighlight %}

Is it possible to perform a sequence of moves to solve the task and
can you write a short program to prove it?

## The solutions

First out, an *OCaml* program written by Anton T:

{% highlight ocaml %}

type place = L | R | E

let search state moves final =
  let rec loop state trace =
      if final state then Some (state :: trace)
      else loops (moves state) (state :: trace)
  and loops states trace =
    match states with
    | [] -> None
    | x :: xs ->
      match loop x trace with
      | Some r -> Some r
      | None -> loops xs trace in
  loop state []

let init = [R; R; R; E; L; L; L]  
let final = function [L; L; L; E; R; R; R] -> true | _ -> false

let anywhere s f =
  let rec loop acc s =
    match f s, s with
    | Some r, _ -> Some (List.rev acc @ r)
    | None, x :: xs -> loop (x :: acc) xs
    | _ -> None in
  loop [] s
  
let move_r s = anywhere s (function R :: E :: x -> Some (E :: R :: x) | _ -> None)

let move_l s = anywhere s ( function E :: L :: x -> Some (L :: E :: x) | _ -> None)
let jump_r s = anywhere s (function R :: x :: E :: y -> Some (E :: x :: R :: y) | _ -> None)
let jump_l s = anywhere s (function E :: x :: L :: y -> Some (L :: x :: E :: y) | _ -> None)

let all_moves = [move_r; move_l; jump_r; jump_l]
let list_of_opt = function None -> [] | Some x -> [x]
let choose f xs = List.concat (List.map (fun x -> list_of_opt (f x)) xs)
let moves s = choose (fun m -> m s) all_moves
let solution = search init moves final  

{% endhighlight %}

Perhaps most interesting here is the `anywhere` function which abstracts
the recursive pattern and allows for non-recursive definitions of *rules*.

The next one is from Gyorgy F. who submitted the following *Clojure* code:

{% highlight clojure %}
(ns joel-challenge.core
  (:gen-class))
(def initial-position [:> :> :> :_ :< :< :<])
(def target-position [:< :< :< :_ :> :> :>])
(defn empty-in-position [position]
  (first (keep-indexed (fn [idx slot]
                         (when  (= slot :_)
                           idx))
                       position)))
(defn step-right [position]
  (let [empty-slot (empty-in-position position)]
    (when-let [left-slot  (when  (> empty-slot 0)
                            (position (- empty-slot 1)))]
      (when (= :> left-slot)
        (assoc position (- empty-slot 1) :_  empty-slot :>)))))
(defn step-left [position]
  (let [empty-slot (empty-in-position position)]
    (when-let [right-slot  (when  (> (count position) (+ empty-slot 1))
                             (position (+ empty-slot 1)))]
      (when (= :< right-slot)
        (assoc position (+ empty-slot 1) :_ empty-slot :<)))))
(defn jump-right [position]
  (let [empty-slot (empty-in-position position)]
    (when-let
     [left-slot (when (> empty-slot 1)
                  (position (- empty-slot 2)))]
      (when (= :> left-slot)
        (assoc position (- empty-slot 2) :_ empty-slot :>)))))
(defn jump-left [position]
  (let [empty-slot (empty-in-position position)]
    (when-let
     [right-slot (when (> (count position) (+ empty-slot 2))
                   (position (+ empty-slot 2)))]
      (when (= :< right-slot)
        (assoc position (+ empty-slot 2) :_ empty-slot :<)))))
(defn generate-moves [position]
  (vec ((juxt step-left step-right jump-left jump-right) position)))
(defn generate-move-tree-node [position]
  (when position
    {:position position  :children (generate-moves position)}))
(defn generate-move-tree [{:keys [:position :children]}]
  (when-let [child-nodes (map generate-move-tree-node children)]
    (let [results  (for [child child-nodes]
                     (generate-move-tree child))]
      (when position
        {:position position :children (remove nil? results)}))))
(defn solutions [init target]
  (let [tree (generate-move-tree  (generate-move-tree-node init))]
    (letfn [(walk [{:keys [:position :children] :as node}]
              (loop [current {:path [position] :node (first children)}
                     next  (map
                            (fn [x] {:path [position] :node x})
                            (rest children))
                     results []]
                (if-let [next-node (first (:children (:node current)))]
                  (let [path (:path current)
                        new-path (conj path (:position (:node current)))
                        to-add (map (fn [x] {:path new-path :node x})
                                    (rest (:children (:node current))))
                        remaining (concat to-add next)
                        ]
                    (recur {:path new-path :node next-node} remaining results))
                  (if-let [new (first next)]
                    (if (= (:position (:node  current)) target)
                      (recur new (rest next) (into results [(conj (:path current) (:position (:node current)) )]))
                      (recur new (rest next) results))
                    (if (= (:position (:node current)) target)
                      (into results [(:path current)])
                         results)))))]
      (walk tree))))
(defn print-solutions [init target]
  (doseq [solution (solutions init target)]
    (println "-------------------")
    (doseq [row solution]
      (println row))
    )
  )
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (print-solutions initial-position target-position))
		
{% endhighlight %}

Here, a tree structure is built up where a node corresponds
to a configuration (arrangement of mice) and its children to 
all states reachable by making one feasible move.

Over to Haskell, Niclas B. contributed with this program:

{% highlight haskell %}
import Data.Maybe

data Mice = L | R deriving (Eq)
data Config = Config [Mice] [Mice] deriving (Eq)

instance Show Config where
   show (Config ls rs) = (show (reverse ls)) ++ "_" ++ (show rs)

instance Show Mice where
   show R = ">"
   show L = "<"
   showList cs = (\s -> concat (map (\m -> show m) cs))

possibleMoves1 (Config (R:ls) rs) = Just (Config ls (R:rs))
possibleMoves1 c = Nothing
possibleMoves2 (Config ls (L:rs)) = Just (Config (L:ls) rs)
possibleMoves2 _ = Nothing
possibleMoves3 (Config (l:R:ls) rs) = Just (Config ls (l:R:rs))
possibleMoves3 _ = Nothing
possibleMoves4 (Config ls (r:L:rs)) = Just (Config (r:L:ls) rs)
possibleMoves4 _ = Nothing
possibleMoves :: Config -> [Config]
possibleMoves c = catMaybes [possibleMoves1 c,
                             possibleMoves2 c,
                             possibleMoves3 c,
                             possibleMoves4 c]

goalConfig = Config [L,L,L] [R,R,R]
initConfig = Config [R,R,R] [L,L,L]

seqToGoal :: Config -> [[Config]] -> [[Config]]
seqToGoal c ms 
	| c == goalConfig = [c]:ms
	| m == [] = []           
	| otherwise = (map (\p -> concat (seqToGoal p ([c]:ms))) m)
  where
   m = possibleMoves c

main :: IO()
main = mapM_ print sequence
  where
    sequence = reverse (firstSolution)
    firstSolution = head (seqToGoal (initConfig) [])

{% endhighlight %}

What sticks out to me is how the configuration is represented by two lists
rather than one.  This allows for concise rule definitions (`possibleMoves`). 
Note that the first list of a configuration is in reverse order.

Sebastian O. first submitted a hand crafted solution but then also brought 
some C# to the table:

{% highlight csharp %}
using System;
using System.Collections.Generic;
using System.Linq;

namespace ChristmasLeaning
{
    internal class Program
    {
        private enum SlotState
        {
            BLANK,
            LEFT_LEANING,
            RIGHT_LEANING
        }

        private static void ValidateConfiguration(List<SlotState> configuration)
        {
            if (configuration == null)
                throw new ArgumentNullException(nameof(configuration));
            if (configuration.Count != 7)
                throw new Exception();
            if (configuration.Count(s => s == SlotState.BLANK) != 1)
                throw new Exception();
            if (configuration.Count(s => s == SlotState.RIGHT_LEANING) != 3)
                throw new Exception();
            if (configuration.Count(s => s == SlotState.LEFT_LEANING) != 3)
                throw new Exception();
        }

        private static bool IsFinalConfiguration(List<SlotState> configuration)
        {
            var isFinalState =
                configuration.Count == 7
                && configuration[0] == SlotState.LEFT_LEANING
                && configuration[1] == SlotState.LEFT_LEANING
                && configuration[2] == SlotState.LEFT_LEANING
                && configuration[3] == SlotState.BLANK
                && configuration[4] == SlotState.RIGHT_LEANING
                && configuration[5] == SlotState.RIGHT_LEANING
                && configuration[6] == SlotState.RIGHT_LEANING;

            return isFinalState;
        }

        // Returns a list of possible new configurations given a configuration.
        private static List<List<SlotState>> GetPossibleConfigurations(List<SlotState> configuration)
        {
            var possibleConfigurations = new List<List<SlotState>>();
            var blankPosition = configuration.FindIndex(s => s == SlotState.BLANK);

            var newConfiguration = SwapSlotStateIfPosition2Match(configuration, blankPosition, blankPosition - 2, SlotState.RIGHT_LEANING);
            if (newConfiguration != null)
                possibleConfigurations.Add(newConfiguration);

            newConfiguration = SwapSlotStateIfPosition2Match(configuration, blankPosition, blankPosition - 1, SlotState.RIGHT_LEANING);
            if (newConfiguration != null)
                possibleConfigurations.Add(newConfiguration);

            newConfiguration = SwapSlotStateIfPosition2Match(configuration, blankPosition, blankPosition + 1, SlotState.LEFT_LEANING);
            if (newConfiguration != null)
                possibleConfigurations.Add(newConfiguration);

            newConfiguration = SwapSlotStateIfPosition2Match(configuration, blankPosition, blankPosition + 2, SlotState.LEFT_LEANING);
            if (newConfiguration != null)
                possibleConfigurations.Add(newConfiguration);

            return possibleConfigurations;
        }

        private static List<SlotState> SwapSlotStateIfPosition2Match(List<SlotState> state, int position1, int position2, SlotState matchingSlotState)
        {
            if (position2 >= 0 && position2 < state.Count && state[position2] == matchingSlotState)
            {
                var newConfiguration = new List<SlotState>(state);
                newConfiguration[position1] = matchingSlotState;
                newConfiguration[position2] = SlotState.BLANK;
                return newConfiguration;
            }

            return null;
        }

        private static List<List<List<SlotState>>> Move(List<List<SlotState>> currentSequence)
        {
            var currentConfiguration = currentSequence.Last();

            ValidateConfiguration(currentConfiguration);

            if (IsFinalConfiguration(currentConfiguration))
            {
                return new List<List<List<SlotState>>>
                {
                    new List<List<SlotState>>(currentSequence)
                };
            }

            var possibleConfigurations = GetPossibleConfigurations(currentConfiguration);
            if (!possibleConfigurations.Any())
                return new List<List<List<SlotState>>>
                {
                    new List<List<SlotState>>(currentSequence)
                };

            var newSequences = possibleConfigurations.Select(nextConfiguration => new List<List<SlotState>>(currentSequence) {nextConfiguration});

            var allMoves = new List<List<List<SlotState>>>();
            foreach (var sequence in newSequences)
            {
                var moves = Move(sequence);
                allMoves.AddRange(moves);
            }

            return allMoves;
        }

        private static char SlotStateToChar(SlotState slotState)
        {
            if (slotState == SlotState.BLANK)
                return '_';
            if (slotState == SlotState.LEFT_LEANING)
                return '<';
            if (slotState == SlotState.RIGHT_LEANING)
                return '>';

            throw new Exception();
        }

        private static void PrintConfiguration(List<SlotState> configuration)
        {
            var s = string.Concat(configuration.Select(SlotStateToChar));
            Console.WriteLine(s);
        }

        private static void PrintSequence(List<List<SlotState>> sequence)
        {
            foreach (var configuration in sequence)
            {
                PrintConfiguration(configuration);
            }
        }

        static void Main(string[] args)
        {
            Console.WriteLine("The leaning mice");

            var initialConfiguration = new List<SlotState>
            {
                SlotState.RIGHT_LEANING,
                SlotState.RIGHT_LEANING,
                SlotState.RIGHT_LEANING,
                SlotState.BLANK,
                SlotState.LEFT_LEANING,
                SlotState.LEFT_LEANING,
                SlotState.LEFT_LEANING
            };

            var startSequence = new List<List<SlotState>>(new List<List<SlotState>> { initialConfiguration });
            try
            {
                var sequences = Move(startSequence);
                var successfulSequences = sequences.Where(sequence => IsFinalConfiguration(sequence.Last())).ToList();

                foreach (var sequence in successfulSequences)
                {
                    Console.WriteLine("Successful sequence!");
                    PrintSequence(sequence);
                    Console.WriteLine();
                }

                Console.WriteLine("Done!");
            }
            catch (Exception e)
            {
                Console.WriteLine("Exception!");
            }

            Console.ReadLine();
        }
    }
}

{% endhighlight %}

What's notable here (besides the number of lines :)) is how well it reads.
It is utilizing the position of the empty slot for defining the set of feasible moves
without traversing the configuration list.

Next up is Daniel W. with some *FSharp*:

{% highlight ocaml %}

module MarchingMice

open System

// L leans left, R leans right
type Mouse = L | R

type HoleDirection = Leftwards | Rightwards

let miceToString (leftQueue, rightQueue) =
  sprintf "%A _ %A" (List.rev leftQueue) rightQueue

let display turn mice =
  printfn "Turn %d:  %s" turn (miceToString mice)

/// Unsolvable problems will cause an infinite loop
/// (e.g. when there is a left-leaning mouse on the right side
/// of two consecutive right-leaning mice)
let rec marchMice turn direction mice =
  // -- Helpers
  let all mouse = List.forall (fun m -> m = mouse)
  let newTurn direction mice =
    display turn mice
    Console.ReadLine() |> ignore
    marchMice (turn+1) direction micej
  // -- Next step
  match (direction, mice) with
  // Completed?
  | (_, (xs, ys)) when (all L xs) && (all R ys) ->
    printfn "Completed!"
    mice
  | (Leftwards, (L::R::xs, ys))  -> newTurn Leftwards (xs, L::R::ys)
  | (Leftwards, (R::xs, ys))     -> newTurn Rightwards (xs, R::ys)
  | (Leftwards, _)               -> marchMice turn Rightwards mice
  | (Rightwards, (xs, R::L::ys)) -> newTurn Rightwards (R::L::xs, ys)
  | (Rightwards, (xs, L::ys))    -> newTurn Leftwards (L::xs, ys)
  | (Rightwards, _)              -> marchMice turn Leftwards mice

let startLineUp = (R::R::R::[], L::L::L::[])
// Example of solvable when starting rightwards, but not leftwards
//let startLineUp = (R::L::[], R::L::[])

[<EntryPoint>]
let main argv = 
  display 0 startLineUp
  Console.ReadLine() |> ignore
  marchMice 1 Rightwards startLineUp |> ignore
  Console.ReadLine() |> ignore
  0 // return an integer exit code

{% endhighlight %}

Daniel also appearantly managed to solve the problem without turning to computers
and the program is codifying a strategy rather than searching and backtracking
for solutions.

Oszkar J. then submitted the follwoing versioni in *JavaScript*:

{% highlight javascript %}
const init = ['>', '>', '>', '_', '<', '<', '<']
const goal = ['<', '<', '<', '_', '>', '>', '>']

const score = state => state.map((s, i) => s === goal[i] ? 1 : 0).reduce((acc, s) => acc + s, 0)
const nexts = state =>
  state.reduce((acc, c, i, arr) => {
    if (c === '>' && arr[i + 1] === '_') {
      let a = arr.slice()
      a[i + 1] = c
      a[i] = '_'
      acc.push(a)
    }
    if (c === '>' && arr[i + 2] === '_') {
      let a = arr.slice()
      a[i + 2] = c
      a[i] = '_'
      acc.push(a)
    }
    if (c === '<' && arr[i - 1] === '_') {
      let a = arr.slice()
      a[i - 1] = c
      a[i] = '_'
      acc.push(a)
    }
    if (c === '<' && arr[i - 2] === '_') {
      let a = arr.slice()
      a[i - 2] = c
      a[i] = '_'
      acc.push(a)
    }
    return acc
  }, [])

const search = (state, depth = 50) => {
  const nextStates = nexts(state)
  if (nextStates.length === 0 || depth <= 0) {
    return { score: score(state), state: [state] }
  } else {
    return nextStates
      .map(s => search(s, depth - 1))
      .reduce((max, result, i, arr) => {
        result.state.push(state)
        return max.score < result.score ? result : max
      }, { score: -Infinity })
  }
}
{% endhighlight %}

Note the clever way of accumulating the neighbouring states by folding over
a configuration array in the `nexts` function.

Also striking how seemingly functional *JavaScript* has become, particularly with 
the addition of `const` and `=>` keywords (these were both new to me).

Tamas K. further diversified the solution space by submitting the these lines of *R*:

{% highlight r %}
require(rlist)

start <- c("R","R","R","E","L","L","L")
end <-  c("L","L","L","E","R","R","R")

moves <- function(input) {
  list <- unlist(input)
  e <- which(list=='E')
  if (e-1>0 && list[e-1]=="R") {node1 <- replace(list,c(e-1,e),list[c(e,e-1)])} else {node1 <- list}
  if (e-2>0 && list[e-2]=="R") {node2 <- replace(list,c(e-2,e),list[c(e,e-2)])} else {node2 <- list}
  if (e+1<=length(list) && list[e+1]=="L") {node3 <- replace(list,c(e+1,e),list[c(e,e+1)])} else {node3 <- list}
  if (e+2<=length(list) && list[e+2]=="L") {node4 <- replace(list,c(e+2,e),list[c(e,e+2)])} else {node4 <- list}
  return(list(node1,node2,node3,node4))
}

step <- function(input) {
  input <- unique(list.flatten(input))
  rslt <- lapply(input,moves)
  return(rslt)
}

find <- function(input){
  if (length(list.search(input,all(.==unlist(end))))>0) return(list.search(input,all(.==unlist(end))))
  else return(find(step(input)))
}

find(step(moves(start)))
{% endhighlight %}

Another very concise program! Similar to Sebastian's solution in how it's
using the position of the empty slot in the `moves` function. The program does 
not preserve the configuration states however.

Finally, here's the (Haskell) code I ended up writing myself:

{% highlight haskell %}
import qualified Data.Maybe as M

data Slot   = R | L | E deriving Eq
type State  = [Slot] 

instance Show Slot where
    show R = ">"
    show L = "<"
    show E = "_"

moveRight :: State -> [State]
moveRight []                = []
moveRight (R : E : sts)     = [E : R : sts]
moveRight (R : L : E : sts) = [E : L : R : sts]
moveRight (R : R : E : sts) = [E : R : R : sts, R : E : R : sts] 
moveRight (st : sts)        = [st : sts' | sts' <- moveRight sts]

moveLeft :: State -> [State]
moveLeft = map invert . moveRight . invert
  where
    invert = reverse . map flipDir
    flipDir R = L
    flipDir L = R
    flipDir E = E

moves :: State -> [State]
moves st = moveLeft st ++ moveRight st

solutions :: State -> Maybe [State]
solutions st
  | st == [L, L, L, E, R, R, R] =
    Just [st]
  | otherwise = 
    M.listToMaybe [ st : sts | Just sts <- map solutions $ moves st]

main :: IO ()
main = do
  case solutions [R,R,R,E,L,L,L] of
    Nothing   -> putStrLn "No solutions"
    Just ss   -> mapM_ print ss
{% endhighlight %}

### Conclusion
The exercise is relatively straight forward and can be tackled
by a backtracking algorithm exploring the full configuration space. Most submissions
from above are implementations of this algorithm. The variety stems
from how a configuration is represented and the encoding of feasible moves.

As for the actual solution to the puzzle itself, here's a sequence of 
moves leading up to the final configuration:

{% highlight haskell %}
[>,>,>,_,<,<,<]
[>,>,>,<,_,<,<]
[>,>,_,<,>,<,<]
[>,_,>,<,>,<,<]
[>,<,>,_,>,<,<]
[>,<,>,<,>,_,<]
[>,<,>,<,>,<,_]
[>,<,>,<,_,<,>]
[>,<,_,<,>,<,>]
[_,<,>,<,>,<,>]
[<,_,>,<,>,<,>]
[<,<,>,_,>,<,>]
[<,<,>,<,>,_,>]
[<,<,>,<,_,>,>]
[<,<,_,<,>,>,>]
[<,<,<,_,>,>,>]
{% endhighlight %}

Thanks to everyone who participated!
