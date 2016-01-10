{-# OPTIONS_GHC -fno-warn-tabs #-}

import System.Directory
import Text.Regex.PCRE
import System.Process
import System.IO
import Text.Printf

main = do
	contents <- getDirectoryContents "tests"
	let pbfiles = map ("tests/" ++ ) $ filter (=~ "\\.pb$") contents
	let runTest fname = do
		putStr (printf "Running test %s..." fname)
		ourOutput <- readFile (fname) >>= readProcess "./test" []
		referenceOutput <- readFile (fname ++ ".html")
		if (ourOutput == referenceOutput) then
			putStrLn "ok"
			else do
			-- Make a wdiff
			(ourName, ourFile) <- openTempFile "/tmp" "our.pb"
			hPutStr ourFile ourOutput
			hClose ourFile
			(_, wdiffOutput, _) <- readProcessWithExitCode "wdiff" ["-t", fname ++ ".html", ourName] ""
			removeFile ourName
			putStrLn "error. wdiff follows"
			putStrLn wdiffOutput
			putStrLn ""
	mapM (runTest) pbfiles