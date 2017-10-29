module Main where
import Mus.Solve
import Data.SBV
import Data.SBV.Control
import Control.Monad
import qualified Haskore as H
import Mus.Play

main = do
    mels <- runSMT $ genMelody 1
    mapM (\(i, mel) -> makeMidi ("test_" ++ (show i) ++ ".mid") mel) (zip [0..(length mels)-1] mels)
