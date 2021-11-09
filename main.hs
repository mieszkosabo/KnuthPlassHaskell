module Main where

import Control.Monad.List (forM_, when)
import Control.Monad.Trans.State

-- WORK IN PROGRESS
main :: IO ()
main = print $ getStretchability (Penalty 1 42 False)

type Width = Double

type Fitness = Int

infinity :: Double
infinity = 1024

negativeInfinity :: Double
negativeInfinity = negate infinity

type Paragraph = [Specification]

data Specification
  = Box {getWidth :: Width}
  | Glue
      { getWidth :: Width,
        getStretchability :: Width,
        getShrinkability :: Width
      }
  | Penalty
      { getPenalty :: Double,
        getWidth :: Width,
        getFlag :: Bool
      }

isBox :: Specification -> Bool
isBox Box {} = True
isBox _ = False

isGlue :: Specification -> Bool
isGlue Glue {} = True
isGlue _ = False

isPenalty :: Specification -> Bool
isPenalty Penalty {} = True
isPenalty _ = False

data Node
  = Nil
  | Node
      { position :: Int,
        line :: Int,
        fitness :: Fitness,
        totalWidth :: Width,
        totalStretch :: Width,
        totalShrink :: Width,
        totalDemerits :: Double,
        previous :: Node,
        next :: Node
      }

toPairsWithRepeatedFirstEl :: [a] -> [(a, a)]
toPairsWithRepeatedFirstEl [] = []
toPairsWithRepeatedFirstEl (x : xs) = zip (x : x : xs) (x : xs)

data GlobalState = GS
  { activeNode :: Node,
    passiveNode :: Node,
    widthSum :: Width,
    stretchSum :: Width,
    shrinkSum :: Width
  }

initialState :: GlobalState
initialState =
  GS
    { activeNode =
        Node
          { position = 0,
            line = 0,
            fitness = 1,
            totalWidth = 0,
            totalStretch = 0,
            totalShrink = 0,
            totalDemerits = 0,
            previous = Nil,
            next = Nil
          },
      passiveNode = Nil,
      widthSum = 0,
      stretchSum = 0,
      shrinkSum = 0
    }

type KnuthM = State GlobalState

handleSpecification :: (Specification, Specification) -> KnuthM ()
handleSpecification (prev, curr) = case curr of
  Box w -> do
    state <- get
    put $ state {widthSum = widthSum state + w}
  Glue w stretch shrink -> do
    when (isBox prev) mainLoop
    state <- get
    put $
      state
        { widthSum = widthSum state + w,
          stretchSum = stretchSum state + stretch,
          shrinkSum = shrinkSum state + shrink
        }
  Penalty p _ _ ->
    when (p /= infinity) mainLoop

knuthPlass :: Paragraph -> KnuthM ()
knuthPlass paragraph = do
  forM_
    (toPairsWithRepeatedFirstEl paragraph)
    handleSpecification

mainLoop = undefined