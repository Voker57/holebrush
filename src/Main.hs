module Main where

import qualified Data.Text.IO as TI
import System.Console.GetOpt
import System.Environment (getArgs)
import Text.Holebrush
import System.IO
import System.Exit
import qualified Data.Text as T

data Opts = Opts { disableImages :: Bool, disableLinks :: Bool }

options :: [OptDescr (Opts -> Opts)]
options = [
	Option [] ["no-images"]
		(NoArg (\o -> o {disableImages = True}) ) "Disable images",
	Option [] ["no-links"]
		(NoArg (\o -> o {disableLinks = True})) "Disable links"
	]

defaultOptions = Opts { disableImages = False, disableLinks = False }

main = do
	args <- getArgs
	let (optz, argz, errs) = getOpt Permute options args
	let opts = processOptions defaultOptions optz
	let ourParseFlags = (mixIf NoImages (disableImages opts)) ++ (mixIf NoLinks (disableLinks opts))
	input <- TI.getContents
	case toHtml (T.strip input) ourParseFlags of
		Right text -> do
			TI.putStrLn text
		Left err -> do
			hPutStrLn stderr ("Parsing error: " ++ show err)
			exitWith $ ExitFailure 1