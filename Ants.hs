module Ants where

--------------------------------------------------------------------------------
-- This file should contain your complete work for the first coursework of 
-- CS141 Functional Programming.
-- 
-- USER ID: 5514769
--
-- Before starting on this file, ensure that you have read the specification IN
-- ITS ENTIRETY and that you understand everything that is required from a good
-- solution.


-- DECLARATIONS FROM ME:
-- I have derived an Enum instance for Direction

-- I used this source to find out how to concatenate strings.
-- https://stackoverflow.com/questions/10755852/how-to-concat-two-io-strings-in-haskell

--------------------------------------------------------------------------------

import Ants.Types
import Hatch


-- | Uncomment these two lines once you get to Exercise 6.
import Data.Set (Set)
import qualified Data.Set as Set




{-| 
  Ex. 1: Implement a Show instance for LineState.

  (The pipe character and full block character are | and █)

  I initially did not write the cellToString method and tried to do recursion
  on the LineState. This was a bad idea as it made the cells have separating
  characters in between. I even tried to delete the derivtion of show from
  CellState and write my own. Alas, it did not work properly, so I thought an
  easier appraoch would be to write a new function which would basically be an 
  instance of Show on CellState, though now overwriting it as there may be
  a reason why it is derived.

  cellToSting is implemented with top-level pattern matching. I thought this
  looked better than implemeting with guards or case. I used implicit recursion
  (mapping) to implement the Show instance of LineState, as this was the most 
  concise way of converting all elemens to characters in my opinion. I could use
  map since LineState is basically just an array of CellState datatypes, and I 
  used patternmatching to access this array directly. I was then able to add the
  pipes at the head and tail (researching string concatenation as : would not work)
  to make an appropriate string.
  -}

cellToString :: CellState -> Char
cellToString Off = ' '
cellToString On = '█' 

instance Show LineState where
  show :: LineState -> String
  show (LS cells) = "|" ++ (map cellToString cells) ++ "|"



{-|
  Ex. 2: Implement ruleX, which turns a cell on if it has exactly one neighbour which was on in the previous step.
  The three arguments are the states of the left neighbour, current cell, and right neighbour respectively.

  I used exhaustive cases as the cases I needed to consider were very few. I used guards
  because I thought this required the least amount of code and it was easier for me to keep
  track and make sense of what I was doing. 

  The code just considers the case where only the left cell is on and the right is off, and
  the case where the left cell is off and the right cell is on. If either of these are met,
  the rule returns On, otherwise, it returns off. 
  
  Originally I had thought the idea was to 
  leave it untouched in the otherwise case, so I had returned the value 'c' which was the 
  label of my current cell. I then changed it to a wildcard as it does not matter what the 
  value of the current cell is. 

-}

ruleX :: CellState -> CellState -> CellState -> CellState
ruleX l _ r
  | l == On && r == Off   = On
  | l == Off && r == On   = On
  | otherwise             = Off




{-|
  Ex. 3: Implement applyRule, which, given a rule and a line state,
  applies the rule to each cell in the state and returns the updated state.

  Exercise 3 was difficult to say the least. I initially started using recursion
  before I realised that it was difficult to navigate the edge cases. The recursion
  consists of applying the rule to the first three cells of the line state and then 
  applying the function to a line state starting with the second cell. This would ensure
  I was considering each consecutive set of three cells as I was only shifting by one
  and not three. Cons does not work to concatenate a CellState to a line state, so I 
  made a function lineToCells that uses pattern matching to 'unwrap' the list off
  CellSates from a line state. I had also originally made a 
  function that adds on the Offs to either side, and wrote a helper function which
  would essentially do the job of applyRule, and applyRule was going to apply it 
  to the line state with edges. This proved challenging as I needed to exhaustively
  consider list lengths with the helper function and also applyRule. I thought this 
  would not be very elegant and that it was likely unecessary code.

  I had also then tried to convert a the linestate to a list of its triplets  (e.g. [1..5] would become
  [(1,2,3) (2,3,4) (3,4,5)]) so that I could map the rule over this. I tried to use 
  uncurry on the rule which did not work, so I tried to use my own function. At this 
  point I felt that map may also not work as the inputs were not of the correct
  shape. I also thought this would be doing unecessary computation as the line state
  would be iterated once to convert it to triplets but then again to apply the rule.

  Returnng to my initial idea, I rewrote my applyRule function to concatenate the 
  rule applied to the first cell (so considering the Off at the beginning) to the 
  rule applied to the rest of the line state (using the helper function). I had to 
  concatenate it once separately as if it were part of any recursion in applyRule
  then it would treat every tripelt as if it started with an Off.

  applyRule' (helper function) is only meant to apply to cases where there are three or more cells in the
  line state. applyRule more caters to any length line state, and uses top-level pattern 
  matching to immediately deal with line states of length one or two, adding the edge
  Offs (there are small enough cases that this was pragmatic). In this way, applyRule'
  would not be utilised as it exibhits slightly different behaviour. applyRule' would be
  used when iterating through the line state with greater than two cells. Hence, the case 
  in applyRule' which deals with two cells would be applied when the end of the line state
  is reached. This adds the end Off. Since we want to stop applying rules after this, the 
  case with one cell and no cells return an empty line state.

  Here is some of the code I used for the triplets and mapping idea, if interested:


  listToTriplets :: [a] -> [(a,a,a)]
  listToTriplets [] = []
  listToTriplets [c] = []
  listToTriplets [l,c] = []
  listToTriplets (x:y:z:stuff) = (x,y,z) : listToTriplets (y:z:stuff)

  linestateWithEdges :: LineState -> LineState
  linestateWithEdges (LS cells) = LS (Off : cells ++ [Off])

  uncurryRule :: (CellState -> CellState -> CellState -> CellState) 
              -> (CellState,CellState,CellState) 
              -> CellState
  uncurryRule rule (l,c,r) = rule l c r -}



lineToCells :: LineState -> [CellState]
lineToCells (LS cells) = cells 


applyRule' :: (CellState -> CellState -> CellState -> CellState)
           -> LineState
           -> LineState

applyRule' _ (LS []) = LS []
applyRule' _ (LS [c]) = LS []
applyRule' rule (LS [c,r]) = LS [rule c r Off]
applyRule' rule (LS (l:c:r:cells)) = LS (rule l c r : lineToCells (applyRule' rule (LS (c:r:cells)))) 

applyRule :: (CellState -> CellState -> CellState -> CellState)
          -> LineState
          -> LineState
applyRule _ (LS []) = LS []
applyRule rule (LS [c]) = LS [rule Off c Off]
applyRule rule (LS [c,r]) = LS (rule Off c r : [rule c r Off])
applyRule rule (LS (l:c:cells)) = LS (rule Off l c : lineToCells (applyRule' rule (LS (l:c:cells)))) 




{-|
  Ex. 4: Implement the loopedAt function, which takes a rule and a starting configuration,
  and returns the number of the iteration at which the automaton first revisits a state.

  I needed a function which worked with a list of all the previous linestates. The idea
  was to return the length of this list when the next state was already in the list. Given 
  that this includes state 0, there is no need to add or subtract 1 from the length of the 
  list. Haskell has functions that can identify is an element is in a list, so I could not
  append this last state to the list itself or it would return True every time. Hence I 
  evaluated it separately (though it would be possible, it would be trickier and less 
  elegant to implement a function working with al the elements except the last).

  I could have used cons to append to the front of the list in prevStates, though I 
  found it more intitive to append the last state to the end. This did not require any
  major changes or much extra code, so I was not hesitant to implement it. This allows me
  to verify the code works for other cases.

  The prevStates function is written with guards as it was the neatest in my opinion,
  plus top-Level patern matching would not be applicable. This function also uses
  recursion, similar to the factorial function, where the list input into it grows
  each call. I was able to use the elem function as LineState derives Eq. The 
  'base case' in this recursion was when the next state appears in the list of 
  previous states and each call it added the next state to the end of the list, so
  the function could evaluate the state after that. Initially, a singleton of the initial
  state is passed in by the loopedAt function, which is where the recursion starts.

-}

prevStates :: (CellState -> CellState -> CellState -> CellState)
                -> [LineState]
                -> [LineState]

prevStates rule linestates 
  | applyRule rule (last linestates) `elem` linestates    = linestates
  | otherwise    = prevStates rule (linestates ++ [applyRule rule (last linestates)]) 


loopedAt :: (CellState -> CellState -> CellState -> CellState)
  -> LineState
  -> Int
loopedAt rule line = length (prevStates rule [line])



{-|
  Ex. 5: Implement allRules, which returns all 256 possible rules.

  tupleToFunc is a higher-order fuction that uses lambdas to generate functions. 
  It is used to take in a distinct tuple and generates a function taking in a 
  tuple of three cellstates which then gives a unique output combination for each together.
  I initially tried to use guards for this and tried to generate a curried function, which 
  gave a parse error unfortunately. Using case ... of worked, which I only knew how to implement
  for a single variable, hence I established the input of the generated function as a tuple.

  curryRule simply takes a rule function from tupleToFunc and gives back a curried version of
  that function. I essentially replicated the definition from the lecture notes on how curry is
  implemented.

  All that is left is to generate all distinct 8-tuples containing the different combinations of 
  cell states, and make them into rules with a list comprehension. Initially I created allTuples by
  itself and fed it into the list comprehension where all the rules are generated, however, I found 
  this a good opportunity to use the where keyword in Haskell, as this made it more obvious what the
  purpose of the list was in the first place. I perhaps could have tried to write a list comprehension
  inside another list comprehension, though this would have made the code less elegant and I am not 
  sure if it would have worked as intended.

  Though this code takes many lines, this was the most intuitive way to approach the problem with my
  knowledge and skillset at this point in time.

  -}


tupleToFunc :: (CellState, CellState, CellState, CellState, CellState, CellState, CellState, CellState)
             -> ((CellState, CellState, CellState) -> CellState)

tupleToFunc (a,b,c,d,e,f,g,h) = \ cells -> case cells of
  (Off, Off, Off) -> a
  (Off, Off, On)  -> b
  (Off, On, Off)  -> c
  (Off, On, On)   -> d
  (On, Off, Off)  -> e
  (On, Off, On)   -> f
  (On, On, Off)   -> g
  (On, On, On)    -> h

curryRule :: ((CellState, CellState, CellState) -> CellState) 
              -> (CellState -> CellState -> CellState -> CellState)
curryRule rule = \l c r -> rule (l,c,r)


allRules :: [ CellState -> CellState -> CellState -> CellState ]
allRules = [curryRule (tupleToFunc tup) | tup <- allTuples] 
  where 
    allTuples :: [(CellState, CellState, CellState, CellState, CellState, CellState, CellState, CellState)]
    allTuples = [(a,b,c,d,e,f,g,h) | a <- [Off,On],
                                     b <- [Off,On],
                                     c <- [Off,On],
                                     d <- [Off,On],
                                     e <- [Off,On],
                                     f <- [Off,On],
                                     g <- [Off,On],
                                     h <- [Off,On]]

{-|
  Ex. 6: Implement initialState, which returns the initial configuration of Langton's Ant.
-}
initialState :: AntState
initialState = AS West (0,0) Set.empty



{-|
  Ex. 7: Define the functions leftOf and rightOf, which return the direction to the left and right of the given direction, respectively. Follow the additional constraints given in the specification, and answer the written question here.

  I prefer pattern matching as it visually makes more sense to me. 
  It reflects more of a map of one direction to the other, like a transformation. 
  However, guards makes it look a bit more confusing with the equals signs. Although
  a programmer would definitely understand what is going on, I find pattern-matching 
  more understandable in this context and easier to look at, especially since it uses
  fewer characters and less repetition.
  
  Moreover, the IDE complained at me when I did not give it an 'otherwise' statement for 
  guards. I chose to rewrite it with an 'otherwise' (though initially I did not) for
  completeness. This makes the code slightly less easy to understand too.

  Ultimately, I prefer pattern-matching as it is more elegant, easier to comprehend, and
  more flexible. 
-}

leftOf :: Direction -> Direction
leftOf dir = case dir of
  North -> West
  West  -> South 
  South -> East
  East  -> North

rightOf :: Direction -> Direction
rightOf dir 
  | dir == North  = East
  | dir == East   = South
  | dir == South  = West
  | otherwise   = North



{-|
  Ex. 8: Implement the step function, which takes the ant state and applies the logic given in the specification to return the new ant state.

  I knew that step would have to be a composition of functions that alter 
  different attributes of the state. I initially was going to write three separate
  functions, one for each bullet point detailed in the spec. However, I realised
  that the first two modifications (turning and flipping) were both reliant on the 
  current colour of the square. Hence, I wrote one function for both, called turnNchurn
  ("turn and churn" - I wanted to call it 'turnNburn' but the former is closer to what the ant
  actually performs). 
  Initially I was going to use guards, then realised that pattern-matching would save on
  some repetition. However, given the cases were boolean, I thought an if ... then ... else
  statement would be most appropriate as it enhances readability.

  The second function moves the ant forward. This modifies the state based on the direction
  so it makes sense to make it a separate function to the prior, else there would have been too many cases
  to analyse in a single function. I used pattern-matching as I realised I liked it more
  than guards (which was my default until Ex 7) since it reduces repetition, though it does
  come with the insinuation of a map/transformation which could potentially be confusing, though
  not as much to those who understand the syntax.

  Composing these functions had to be done in the correct order. It would be unwise for the 
  ant to move before turning & flipping as it would then not perform the correct operations
  for the square it moved from since it analyses its current square. Technically this could 
  be implemented though it would be far less elegant.

-}
step :: AntState -> AntState
step =  moveForward . turnNchurn
  where
    turnNchurn :: AntState -> AntState
    turnNchurn (AS direction position bSqrs) = if position `Set.member` bSqrs 
      then AS (leftOf direction) position (position `Set.delete` bSqrs)
      else AS (rightOf direction) position (position `Set.insert` bSqrs) 

    moveForward :: AntState -> AntState
    moveForward (AS direction (x,y) bSqrs) = case direction of
      North -> AS direction (x, y+1) bSqrs
      South -> AS direction (x, y-1) bSqrs
      East  -> AS direction (x+1, y) bSqrs
      West  -> AS direction (x-1, y) bSqrs

    

      




{-|
  Ex. 9: Visualise the behaviour of Langton's Ant by implementing the "animation" function.
  It takes a number (the step) and you must return a picture that represents the state of the automaton at that step.

  There are no tests for this. You can use `stack run` to test your implementation.

  I used recursion and top-level pattern matching for the findState function, which takes
  an integer n and returns the nth state. I could have used the iterate function and accessed 
  the last value of taking the first n terms, but this would have meant generating an infinite
  list which would take more resources. Although we need to recurse the function n times each time
  we call it, this is the least number of recursions that is possible. Top-level pattern matching
  makes this look clean.

  I originally wrote three getter functions for each parameter into the AntState datatype, though 
  they were not all necessary as the only time I needed a getDirection and a getPosition function
  was a situation where they were used together in the (now unimplemented) addAnt function. Hence 
  I just changed the input parameter to an ant state to implement this function in an appropriately compact manner.
  I then also removed the getBlackSquares function as this was only used once, so I could just 
  implement it more directly in this other function. There are now no getter functions for AntState 
  as I just spelt out the parameters where I needed them, and I found a way to use them all in a 
  single function. 

  I made bSquare the unit square. The dimensions had to be 1x1 since I wrote the step function
  to make it move by 1 unit forward. Consequently, I also had to scale the size of the ant down
  in the animation. Though it is only used in one function, I thought it would be useful to have
  it as its own definition as opposed to just implementing the definition into the function directly.
  It may be used to extend the code for extra functionality by others, e.g. if we wanted to have a
  wall of black squares surrounding the ant.

  Initially I used the addSquare function to just put a black square at a position on the screen
  (it was originally called displaySquare). After writing foldr to use in displayAllSquaresAndAnt, I 
  realised I needed to change the type of the original addSquare function to fit the parameter 
  for foldr, so I decided to make it so that it overlayed the square onto a given image. 
  
  displaySquaresAndAnt comprises of two parts, the part which displays the squares and the part
  which displays the ant. It takes in the current ant state as all parameters are used.
  To display the squares, the function folds over the set of black squares, with the accumulator
  being adding the display of the square at particular coordinates to the rest of the image. The
  base case is the blank image. It was very fortunate that lists can be folded over as this can
  output many squares with one line. We do not need to worry about the order the squares are displayed
  as they are all displayed at once.

  The part of the function adding the ant overlays the ant image onto the current square. Not only does it have to
  put the ant in the correct position, but it also has to be in the correct orientation. To rotate
  the ant's image by the correct angle, I decided to implement an Enum instance of Direction, 
  deriving it by editing the Types.hs file. Conveniently, the constructors were declared in an
  order that permitted me to simply multiply the integer value associated to it by 90 to obtain the 
  appropriate degree of rotation. This was very direct. I could have written a function
  which does the map for me, but the Enum required less code; I thought it looked more elegant; and 
  I was eager to use this Enum feature in Haskell as it seemed a slick and appropriate use for it.
  
  For this funcction, I needed to explicitly type out the parameters of the constructor, as they were
  all used explicitly in the function definition.

 Originally, I implemented the displaying of all the squares and displaying the ant as separate functions.
 I realised that both were only used once, which was together, so I could merge them into a single function.

 Here is the code for the two separate functions:

 addAnt :: AntState -> Image
 addAnt (AS dir (x,y) _) = offset x y (rotate ((fromEnum dir) * 90) (scale 0.1 ant))

 displayAllSquares :: Set (Int, Int) -> Image
 displayAllSquares = foldr addSquare blank

 And the implementation of the animation function:

 animation frame = displayAllSquares (getBlackSquares currentState)
                  <@> addAnt currentState
                    where
                      currentState = findState frame

Both these sub-functions needed the currentState as an argument, which further supported the decision
to merge them both into one.

The animation function itself only needs to display all the black squares and the ant. I could not merge this
with the displaySquaresAndAnt as this function works with the parameters of the ant state, whereas 
implementing this with the animation function would not have provided this information.


-}

findState :: Int -> AntState
findState 0 = initialState
findState n = step (findState (n-1))

bSquare :: Image
bSquare = rect 1 1

addSquare :: (Int, Int) -> Image -> Image
addSquare (x,y) im = offset x y bSquare <@> im

displaySquaresAndAnt :: AntState -> Image
displaySquaresAndAnt (AS dir (x,y) bSqrs) = foldr addSquare blank bSqrs
                                        <@> offset x y (rotate ((fromEnum dir) * 90) (scale 0.1 ant))

animation :: Int -> Image
animation frame = displaySquaresAndAnt (findState frame)
