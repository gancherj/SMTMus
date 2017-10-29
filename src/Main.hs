module Main where
import Mus.Solve
import Data.SBV
import Data.SBV.Control
import Control.Monad
import qualified Haskore as H
import Mus.Play

main = do
    mels <- runSMT $ genMelody 1
    let mus = map arrToHaskore mels
        midis = map H.testMidi mus
    mapM (\(i,midi) -> H.outputMidiFile ("test_" ++ (show i) ++".mid") midi) (zip [0.. (length midis)-1] midis)
