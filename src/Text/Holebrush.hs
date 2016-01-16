module Text.Holebrush (ParseFlag(..), toHtml, dirtyParse)  where

import Text.Holebrush.Parsing
import Text.Parsec
import qualified Data.Text as T
import qualified Data.Set as Set

dirtyParse w s = runParser w (ParserState {parseFlags=Set.fromList []}) "" $ T.pack s

toHtml :: T.Text -> [ParseFlag] -> Either String T.Text
toHtml input givenParseFlags = case runParser document (ParserState { parseFlags = Set.fromList givenParseFlags}) "stdio" $ input of
	Right text -> do
		let piecesCleaner = orphanReaper . widowReaper
		let sanitizedText = map (\t -> case t of Paragraph c pieces -> Paragraph c $ piecesCleaner pieces; Heading l c pieces -> Heading l c $ piecesCleaner pieces; a -> a) text
		let idAssignedText = map (assignId) sanitizedText
		let toc = foldl (tocDeepStart) [] idAssignedText
		let tocInjector ttoc p = case p of TableOfContents cs _ -> TableOfContents cs ttoc; a -> a;
		let tocdText = map (tocInjector toc) idAssignedText
		Right $ T.pack $ render $ tocdText
	Left err -> (Left (show err))