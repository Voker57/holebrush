{- 

% ./test <<< "_______________________________________huita________________________________________hui"                                                                                                                           
<em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em>huita________________________________________hui
</em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em>

-}

module Main where

import Data.List
import Data.Maybe
import Data.Char
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Data.List
import qualified Data.Text as T
import Control.Monad
import Text.Printf
import PairedTag
import qualified Data.Set as Set
import System.Console.GetOpt
import System.Environment (getArgs)

data Opts = Opts { disableImages :: Bool, disableLinks :: Bool }

options :: [OptDescr (Opts -> Opts)]
options = [
	Option [] ["no-images"]
		(NoArg (\o -> o {disableImages = True}) ) "Disable images",
	Option [] ["no-links"]
		(NoArg (\o -> o {disableLinks = True})) "Disable links"
	]

defaultOptions = Opts { disableImages = False, disableLinks = False }

class Renderable a where
	render :: a -> String

data Paragraph = Paragraph CssSpec [Chunk] deriving (Show, Eq)

data CssSpec = CssSpec
	(Maybe String) -- ID
	(Maybe String) -- Class
	(Maybe String) -- Language
	(Maybe String) -- Style
	deriving (Show, Eq)

emptyCssSpec = CssSpec Nothing Nothing Nothing Nothing

data Chunk =
	TagStart String PairedTag CssSpec
	| TagEnd String PairedTag
	| Plaintext String 
	| Image CssSpec String (Maybe String) (Maybe String) 
	| Whitespace 
	| Link CssSpec [Chunk] String (Maybe String)
	| LineBreak
	deriving (Eq, Show)

data ParseFlag = NoLinks | NoImages deriving (Eq, Show, Ord)

data ParserState = ParserState { parseFlags :: Set.Set ParseFlag } deriving (Eq, Show)

instance Renderable CssSpec where
	render (CssSpec mId mClass mLang mStyle) = concat [maybe "" (printf " id=\"%s\"") mId, maybe "" (printf " class=\"%s\"") mClass, maybe "" (printf " lang=\"%s\"") mLang, maybe "" (printf " style=\"%s\"") mStyle]

instance Renderable Paragraph where
	render (Paragraph spec chunks) = "<p" ++ render spec ++ ">" ++ (concat $ map (render) chunks) ++ "</p>"

instance Renderable Chunk where
	render (TagStart _ Italic spec) = "<i" ++ render spec ++ ">"
	render (TagEnd _ Italic) = "</i>"
	render (TagStart _ Bold spec) = "<b" ++ render spec ++ ">"
	render (TagEnd _ Bold) = "</b>"
	render (TagStart _ Strong spec) = "<strong" ++ render spec ++ ">"
	render (TagEnd _ Strong) = "</strong>"
	render (TagStart _ Emph spec) = "<em" ++ render spec ++ ">"
	render (TagEnd _ Emph) = "</em>"
	render (Plaintext s) = s
	render Whitespace = " "
	render (Image spec src altMaybe (Just linkUri)) = render $ Link emptyCssSpec [(Image spec src altMaybe Nothing)] linkUri Nothing
	render (Image spec src altMaybe Nothing) = "<img" ++ render spec ++ " alt=\"" ++ escapeHTML (fromMaybe "" altMaybe) ++ "\" src=\"" ++ escapeHTML src ++ "\" />"
	render (Link spec text uri titleMaybe) = "<a" ++ render spec ++ " href=\"" ++ escapeHTML uri ++ "\"" ++ (case titleMaybe of Just title -> " title=\"" ++ escapeHTML title ++ "\""; Nothing -> "") ++ ">" ++ concat (map (render) text) ++ "</a>"
	render LineBreak = "<br />"

instance (Renderable a) => Renderable [a] where
	render xs = concat $ map (render) xs

voidTry = void . try

mixIf :: a -> Bool -> [a]
mixIf entry condition = if condition then [entry] else []

parseFlagOn :: ParserState -> ParseFlag -> Bool
parseFlagOn state flag = Set.member flag (parseFlags state)

processOptions def = foldl (flip ($)) def

-- Stolen shit
spanList :: ([a] -> Bool) -> [a] -> ([a], [a])

spanList _ [] = ([],[])
spanList func list@(x:xs) =
	if func list
		then (x:ys,zs)
		else ([],list)
	where (ys,zs) = spanList func xs

joinLists :: [a] -> [[a]] -> [a]
joinLists delim l = concat (intersperse delim l)

breakList :: ([a] -> Bool) -> [a] -> ([a], [a])
breakList func = spanList (not . func)

split :: Eq a => [a] -> [a] -> [[a]]
split _ [] = []
split delim str =
	let (firstline, remainder) = breakList (isPrefixOf delim) str
		in 
		firstline : case remainder of
			[] -> []
			x -> if x == delim
				then [] : []
				else split delim 
					(drop (length delim) x)

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new l = joinLists new . split old $ l

-- Really basic. Fix? At least add '?
escapeHTML = concat . map (escapeHTMLChar)
escapeHTMLChar '&' = "&amp;"
escapeHTMLChar '<' = "&lt;"
escapeHTMLChar '>' = "&gt;"
escapeHTMLChar '"' = "&quot;"
escapeHTMLChar a = [a] 

unescapeHTML s = replace "&amp;" "&" . replace "&lt;" "<" . replace "&gt;" ">" . replace "&quot;" "\"" 

isClosingTag :: Chunk -> Bool
isClosingTag (TagEnd _ _) = True
isClosingTag _ = False

isOpeningTag :: Chunk -> Bool
isOpeningTag (TagStart _ _ _) = True
isOpeningTag _ = False

doesClose :: Chunk -> Chunk -> Bool
doesClose (TagStart _ t _) (TagEnd _ t2) = t == t2
doesClose _ _ = False

toPlainText :: Chunk -> String
toPlainText t = case t of
	TagStart originalText _ _ -> originalText
	TagEnd originalText _ -> originalText
	_ -> error "toPlainText called on non-lexem"

-- parsers
specAssembler (CssSpec mId mClass mLang mStyle) (("id", nId, textData):specs) restText = specAssembler (CssSpec (Just nId) mClass mLang mStyle) specs (textData ++ restText)
specAssembler (CssSpec mId (Just tClass) mLang mStyle) (("class", nClass, textData):specs) restText = specAssembler (CssSpec mId (Just $ intercalate " " [tClass, nClass]) mLang mStyle) specs (textData ++ restText)
specAssembler (CssSpec mId mClass mLang (Just tStyle)) (("style", nStyle, textData):specs) restText = specAssembler (CssSpec mId mClass mLang (Just $ intercalate ";" [tStyle, nStyle])) specs (textData ++ restText)
specAssembler (CssSpec mId Nothing mLang mStyle) (("class", nClass, textData):specs) restText = specAssembler (CssSpec mId (Just nClass) mLang mStyle) specs (textData ++ restText)
specAssembler (CssSpec mId mClass mLang Nothing) (("style", nStyle, textData):specs) restText = specAssembler (CssSpec mId mClass mLang (Just nStyle)) specs (textData ++ restText)
specAssembler (CssSpec mId mClass mLang mStyle) (("language", nLang, textData):specs) restText = specAssembler (CssSpec mId mClass (Just nLang) mStyle) specs (textData ++ restText)
specAssembler s ((_, _, textData):xs) restText = specAssembler s xs (textData ++ restText)
specAssembler s [] restText = (s, restText)

cssSpecWRaw = do
	rawSpecs <- many $ choice [try classIdSpec, try languageSpec, try cssStyleSpec]
	return $ specAssembler (CssSpec Nothing Nothing Nothing Nothing) (concat rawSpecs) ""

cssSpec = cssSpecWRaw >>= (return . fst)

many1Till p end = do
	notFollowedBy end
	first <- p
	rest <- manyTill p end
	return (first:rest)

idSpec = do
	char '#'
	many1Till (satisfy (not . isSpace)) (lookAhead $ char ')')

cssClassSpec = do
	many1Till (noneOf "\n") (lookAhead $ choice [string ")", idSpec])

classIdSpec = do
	char '('
	certainlyClassSpec <- cssClassSpec
	maybeIdSpec <- optionMaybe $ try idSpec
	char ')'
	-- Careful reconstruction
	return $ reverse $ [("straytext","","("), ("class", certainlyClassSpec, certainlyClassSpec)] ++ (maybe [] (\a -> [("id", a, '#':a)]) maybeIdSpec) ++ [("straytext", "", ")")]

cssStyleSpec = do
	char '{'
	spec <- many1Till (noneOf "\n")  (char '}')
	-- Careful reconstruction
	return [("style", spec, printf "{%s}" spec)]

-- This should be valid bcp47 language tag, but fuck this spec, let document author ensure that.
languageSpec = do
	char '['
	spec <- many1Till (satisfy ((\a -> (isAlphaNum a) || (a == '-')))) (char ']')
	-- Careful reconstruction
	return [("language", spec, printf "[%s]" spec)]

lineBreak = do
	optionMaybe inlineSpace
	char '\n'
	optionMaybe inlineSpace
	return LineBreak

wordBreak = choice [void space, eof]

wordEndTag = (<?> "word end tag") $ do
	wet <- choice [try italicEnd, try boldEnd, emphEnd, strongEnd]
	lookAhead $ choice [voidTry wordEndTag, void space, eof]
	return wet

nonlinkWordEndTag = (<?> "nonlink word end tag") $ do
	wet <- choice [try italicEnd, try boldEnd, emphEnd, strongEnd]
	lookAhead $ choice [voidTry nonlinkWordEndTag, voidTry linkTitlePart, void space, eof]
	return wet

wordStartTag = (<?> "word start tag") $ do
	wst <- choice [try italicStart, try boldStart, try emphStart, try strongStart]
	lookAhead $ choice [voidTry wordStartTag, voidTry wordPiece]
	return wst

linkEnd = (choice [voidTry wordEndTag, void space, eof, try paragraphBreak])

trailingPunctuation = do
	punctChar <- satisfy (isPunctuation)
	punctChars <- manyTill (satisfy (isPunctuation)) (lookAhead linkEnd)
	return $ Plaintext (punctChar:punctChars)

nonlinkWordAndSpace = do
	wrd <- nonlinkWord
	spc <- optionMaybe whitespace
	return (wrd, spc)

linkUriPart = do
	string "\":"
	uri <- manyTill (satisfy (not . isSpace)) $ lookAhead $ choice [voidTry trailingPunctuation, voidTry linkEnd]
	return uri

link = do
	state <- getState
	when (parseFlagOn state NoLinks) (fail "Links disabled")
	char '"'
	cssSpecV <- cssSpec
	textTuples <- manyTill nonlinkWordAndSpace (lookAhead $ choice [voidTry linkTitlePart, voidTry linkUriPart]) 
	titleString <- optionMaybe $ try linkTitlePart
	uri <- linkUriPart
	let tupleFunc a = case a of
		(s, Nothing) -> s
		(s, Just w) -> s ++ w 
	return $ Link cssSpecV (sanitize $ concat $ map (tupleFunc) textTuples) uri titleString

linkTitlePart = do
	char '('
	str <- manyTill (satisfy (\c -> (not $ isSpace c))) (lookAhead $ try $ do { char ')'; linkUriPart })
	char ')'
	lookAhead linkUriPart
	return str

imageTitlePart = do
	char '('
	str <- manyTill (satisfy (\c -> (not $ isSpace c))) (lookAhead $ try $ do { char ')'; imageEndingBang})
	char ')'
	lookAhead imageEndingBang
	return str

imageLink = do
	char ':'
	str <- many $ satisfy (not . isSpace)
	return str

imageEndingBang = char '!' >> choice [voidTry imageLink, voidTry trailingPunctuation, voidTry wordEndTag, wordBreak]

image = do
	state <- getState
	when (parseFlagOn state NoImages) (fail "Images disabled")
	char '!'
	cssSpecV <- cssSpec
	imageSrcStr <- manyTill (satisfy (\c -> (not $ isSpace c))) (lookAhead $ do { choice [voidTry imageEndingBang, voidTry $ do {imageTitlePart; imageEndingBang}]; when (not $ parseFlagOn state NoLinks) (voidTry $ optionMaybe $ imageLink)})
	imageAltString <- optionMaybe $ try imageTitlePart
	char '!'
	imageLinkString <- if parseFlagOn state NoLinks then
		return Nothing
		else
		optionMaybe $ try imageLink
	return $ Image cssSpecV imageSrcStr imageAltString imageLinkString

italicStart = do
	string "__"
	(cssSpecV, specsText) <- cssSpecWRaw
	notFollowedBy wordBreak
	return $ TagStart ("__" ++ specsText) Italic cssSpecV

boldStart = do
	string "**"
	(cssSpecV, specsText) <- cssSpecWRaw
	notFollowedBy wordBreak
	return $ TagStart ("**" ++ specsText) Bold cssSpecV

emphStart = do
	char '_'
	(cssSpecV, specsText) <- cssSpecWRaw
	notFollowedBy wordBreak
	return $ TagStart ("_" ++ specsText) Emph cssSpecV

strongStart = do
	char '*'
	(cssSpecV, specsText) <- cssSpecWRaw
	notFollowedBy wordBreak
	return $ TagStart ("*" ++ specsText) Strong cssSpecV

emphEnd = do
	char '_'
	return $ TagEnd "_" Emph

strongEnd = do
	char '*'
	return $ TagEnd "*" Strong

italicEnd = do
	string "__"
	return $ TagEnd "__" Italic

boldEnd = do
	string "__"
	return $ TagEnd "__" Bold

inlineSpace = do
	many1 $ satisfy (\c -> isSpace c && c /= '\n')
	return Whitespace

whitespace = manyTill (choice [try lineBreak, inlineSpace]) (lookAhead $ choice [void $ satisfy (not . isSpace), voidTry $ string "\n\n", eof])

plainWord = do
	sc <- satisfy (not . isSpace)
	scs <- manyTill (satisfy (not . isSpace)) (lookAhead $ choice [voidTry wordEndTag, void space, eof, voidTry paragraphBreak, void $ char '"'])
	return (Plaintext (sc:scs))

nonlinkPlainWord = do
	let wordChar = satisfy (\c -> not (isSpace c) && not (c == '"'))
	sc <- wordChar
	scs <- manyTill wordChar (lookAhead $ choice [voidTry nonlinkWordEndTag, void space, eof, voidTry paragraphBreak, voidTry linkTitlePart, voidTry linkUriPart])
	return (Plaintext (sc:scs))

nonlinkWord = do
	starts <- many $ try wordStartTag
	wPiece <- nonlinkWordPiece
	wordPieces <- manyTill nonlinkWordPiece (lookAhead $ choice [void wordBreak, voidTry nonlinkWordEndTag, voidTry linkTitlePart, voidTry linkUriPart])
	ends <- many $ try nonlinkWordEndTag
	return (starts ++ wPiece:wordPieces ++ ends)

nonlinkWordPiece = choice [try image, try trailingPunctuation, try nonlinkPlainWord]

wordPiece = choice [try image, try link, try trailingPunctuation, try plainWord]

word = do
	starts <- many $ try wordStartTag
	wPiece <- wordPiece
	wordPieces <- manyTill wordPiece (lookAhead $ choice [wordBreak, voidTry wordEndTag])
	ends <- many $ try wordEndTag
	return (starts ++ wPiece:wordPieces ++ ends)

document = do
	option () $ void whitespace
	paragraphs <- paragraph `sepBy` paragraphBreak
	option () $ void whitespace
	return paragraphs

paragraphBreak = do
	optionMaybe whitespace
	string "\n\n"
	optionMaybe whitespace
	return ()

wordAndSpace = do
	wrd <- word
	spc <- optionMaybe whitespace
	return (wrd, spc)

paragraph = do
	cssSpecV <- cssSpec
	wordsSpaces <- many wordAndSpace
	let tupleFunc a = case a of
		(s, Nothing) -> s
		(s, Just w) -> s ++ w
	return $ Paragraph cssSpecV $ concat $ map (tupleFunc) wordsSpaces

findOpeningTag' :: [Chunk] -> Integer -> [Chunk] -> Chunk -> Maybe [Chunk]
findOpeningTag' processedChunks skip [] tag = Nothing
findOpeningTag' processedChunks skip (lastChunk:chunks) tag =
	if doesClose lastChunk tag then
		if skip > 0 then
			findOpeningTag' (lastChunk:processedChunks) (skip-1) chunks tag 
			else
			Just (reverse processedChunks ++ (lastChunk:chunks))
		else if tag == lastChunk then
			findOpeningTag' (lastChunk:processedChunks) (skip+1) chunks tag 
		else if isOpeningTag lastChunk then
			findOpeningTag' ((Plaintext $ toPlainText lastChunk):processedChunks) skip chunks tag
		else findOpeningTag' (lastChunk:processedChunks) skip chunks tag 

findOpeningTag = findOpeningTag' [] 0

-- <widow> </orphan>
-- widow reaper assumes that orphans are already killed

orphanReaper = reverse . (orphanReaper' [])

orphanReaper' output [] = output
orphanReaper' output (nextChunk:input) =
	if isClosingTag nextChunk then
		case findOpeningTag output nextChunk of
			Just newOutput -> orphanReaper' (nextChunk:newOutput) input
			Nothing -> orphanReaper' ((Plaintext $ toPlainText nextChunk):output) input
		else
			orphanReaper' (nextChunk:output) input

widowScanner [] ((nextNumber, nextChunk):chunks) = 
	if isOpeningTag nextChunk then
		widowScanner [(nextNumber, nextChunk)] chunks
	else 
		widowScanner [] chunks
widowScanner openedTags [] = openedTags
widowScanner openedTags@((lastNumber, lastOpen):restOpen) ((nextNumber, nextChunk):chunks) =
	if doesClose lastOpen nextChunk then
		widowScanner restOpen chunks
		else if isOpeningTag nextChunk then
			widowScanner ((nextNumber, nextChunk):openedTags) chunks
		else -- We can assume at this point no orphaned closing tags exist
			widowScanner openedTags chunks

widowFilter ([], ((_, chunk):chunks)) = Just (chunk, ([], chunks))
widowFilter (_, []) = Nothing 
widowFilter ((o:orphans), (chunkTuple@(chunkNumber, chunkChunk):chunks)) =
	if o == chunkTuple then Just (Plaintext $ toPlainText chunkChunk, (orphans, chunks)) else Just (chunkChunk, ((o:orphans), chunks))

widowReaper :: [Chunk] -> [Chunk]
widowReaper chunks = let
	numberedChunks = reverse (zip [0..] chunks)
	widowIds = widowScanner [] $ reverse numberedChunks
	in reverse $ unfoldr (widowFilter) (widowIds, numberedChunks)

sanitize = orphanReaper . widowReaper

main = do
	args <- getArgs
	let (optz, argz, errs) = getOpt Permute options args
	let opts = processOptions defaultOptions optz
	let ourParseFlags = (mixIf NoImages (disableImages opts)) ++ (mixIf NoLinks (disableLinks opts))
	input <- getContents
	let rl = runParser document (ParserState { parseFlags = Set.fromList ourParseFlags}) "stdio" $ T.strip $ T.pack input
	print rl
	case rl of
		Right text -> do
			let sanitizedText = map (\(Paragraph c pieces) -> Paragraph c $ orphanReaper $ widowReaper pieces) text
			putStrLn $ render sanitizedText
		Left err -> print err