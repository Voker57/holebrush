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

class Renderable a where
  render :: a -> String

data Paragraph = Paragraph [Chunk] deriving (Show, Eq)

data Chunk = StrongStart | StrongEnd | EmphStart | EmphEnd | ItalicStart | ItalicEnd | BoldStart | BoldEnd | Plaintext String | Image String (Maybe String) (Maybe String) | Whitespace | Link [Chunk] String (Maybe String) deriving (Eq, Show)

instance Renderable Paragraph where
  render (Paragraph chunks) = "<p>" ++ (concat $ map (render) chunks) ++ "</p>"

instance Renderable Chunk where
  render ItalicStart = "<i>"
  render ItalicEnd = "</i>"
  render BoldStart = "<b>"
  render BoldEnd = "</b>"
  render StrongStart = "<strong>"
  render StrongEnd = "</strong>"
  render EmphStart = "<em>"
  render EmphEnd = "</em>"
  render (Plaintext s) = s
  render Whitespace = " "
  render (Image src altMaybe (Just linkUri)) = render $ Link [(Image src altMaybe Nothing)] linkUri Nothing
  render (Image src altMaybe Nothing) = "<img alt=\"" ++ escapeHTML (fromMaybe "" altMaybe) ++ "\" src=\"" ++ escapeHTML src ++ "\" />"
  render (Link text uri titleMaybe) = "<a href= \"" ++ escapeHTML uri ++ "\"" ++ (case titleMaybe of Just title -> "title=\"" ++ escapeHTML title ++ "\""; Nothing -> "") ++ ">" ++ concat (map (render) text) ++ "</a>"

instance (Renderable a) => Renderable [a] where
  render xs = concat $ map (render) xs

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
isClosingTag t = case t of
  ItalicEnd -> True
  BoldEnd -> True
  StrongEnd -> True
  EmphEnd -> True
  _ -> False

isOpeningTag :: Chunk -> Bool
isOpeningTag = isJust . closingTag

closingTag :: Chunk -> Maybe Chunk
closingTag t = case t of
  ItalicStart -> Just ItalicEnd
  BoldStart -> Just BoldEnd
  EmphStart -> Just EmphEnd
  StrongStart -> Just StrongEnd
  _ -> Nothing


toPlainText :: Chunk -> String
toPlainText t = case t of
  ItalicStart -> "__"
  ItalicEnd -> "__"
  EmphStart -> "_"
  EmphEnd -> "_"
  BoldStart -> "**"
  BoldEnd -> "**"
  StrongStart -> "*"
  StrongEnd -> "*"
  Plaintext str -> str
  Image src Nothing Nothing -> "!" ++ src ++ "!"
  Image src (Just altText) Nothing -> "!" ++ src ++ "(" ++ altText ++ ")!"
  Image src altMaybe (Just linkUri) -> (toPlainText $ Image src altMaybe Nothing) ++ ":" ++ linkUri
  Link text uri titleMaybe -> "\"" ++ concat (map (toPlainText) text) ++ (case titleMaybe of Nothing -> ""; Just title -> "(" ++ title ++ ")") ++  "\":" ++ uri

wordBreak = choice [void space, eof]

wordEndTag = (<?> "word end tag") $ do
  choice [italicEnd, boldEnd, emphEnd, strongEnd]

wordStartTag = (<?> "word start tag") $ do
  choice [italicStart, boldStart, emphStart, strongStart]

parText = (word `sepBy` whitespace)

linkEnd = (choice [try wordEndTag >> return (), space >> return (), eof, try paragraphBreak])

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
	uri <- manyTill (satisfy (not . isSpace)) $ lookAhead $ choice [try $ void trailingPunctuation, try $ void linkEnd]
	return uri

link = do
	char '"'
	textTuples <- manyTill nonlinkWordAndSpace (lookAhead $ choice [try $ void $ linkTitlePart, try $ void linkUriPart]) 
	titleString <- optionMaybe $ try linkTitlePart
	uri <- linkUriPart
	let tupleFunc a = case a of
		(s, Nothing) -> s
		(s, Just w) -> s ++ [w] 
	return $ Link (sanitize $ concat $ map (tupleFunc) textTuples) uri titleString

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

imageEndingBang = char '!' >> choice [try $ void imageLink, try $ void trailingPunctuation, try $ void wordEndTag, wordBreak]

image = do
	char '!'
	imageSrcStr <- manyTill (satisfy (\c -> (not $ isSpace c))) (lookAhead $ ((choice [try $ void imageEndingBang, try $ ((void imageTitlePart) >> (void imageEndingBang))]) >> (optionMaybe $ try $ void imageLink)))
	imageAltString <- optionMaybe $ try imageTitlePart
	char '!'
	imageLinkString <- optionMaybe $ try imageLink
	return $ Image imageSrcStr imageAltString imageLinkString

italicStart = do
  string "__"
  notFollowedBy wordBreak
  return ItalicStart

boldStart = do
  string "**"
  notFollowedBy wordBreak
  return BoldStart

emphStart = do
  char '_'
  notFollowedBy wordBreak
  return EmphStart

strongStart = do
  char '*'
  notFollowedBy wordBreak
  return StrongStart

emphEnd = do
  char '_'
  lookAhead $ try (choice [void wordEndTag, void space, eof])
  return EmphEnd

strongEnd = do
  char '*'
  lookAhead $ try (choice [void wordEndTag, void space, eof])
  return StrongEnd

italicEnd = do
  string "__"
  lookAhead $ try (choice [void wordEndTag, void space, eof])
  return ItalicEnd

boldEnd = do
  string "__"
  lookAhead $ try (choice [void wordEndTag, void space, eof])
  return BoldEnd

whitespace = do
  sc <- space
  scs <- manyTill space (lookAhead $ try $ choice [string "\n\n" >> return (), eof, satisfy (not . isSpace) >> return ()])
  return Whitespace

plainWord = do
  sc <- satisfy (not . isSpace)
  scs <- manyTill (satisfy (not . isSpace)) (lookAhead $ try $ choice [wordEndTag >> return (), space >> return (), eof, paragraphBreak, void $ char '"'])
  return (Plaintext (sc:scs))

nonlinkPlainWord = do
  let wordChar = satisfy (\c -> not (isSpace c) && not (c == '"'))
  sc <- wordChar
  scs <- manyTill wordChar (lookAhead $ choice [void $ try wordEndTag, void space, eof, paragraphBreak, void $ try linkTitlePart, void $ try linkUriPart])
  return (Plaintext (sc:scs))

nonlinkWord = do
  starts <- many $ wordStartTag
  wPiece <- nonlinkWordPiece
  wordPieces <- manyTill nonlinkWordPiece (lookAhead $ choice [void wordBreak, void $ try wordEndTag, void $ try linkTitlePart, void $ try linkUriPart])
  ends <- many $ wordEndTag
  return (starts ++ wPiece:wordPieces ++ ends)

nonlinkWordPiece = (choice [try image, try trailingPunctuation, try nonlinkPlainWord]) 
wordPiece = (choice [try image, try link, try trailingPunctuation, try plainWord]) 

word = do
  starts <- many $ wordStartTag
  wPiece <- wordPiece
  wordPieces <- manyTill wordPiece (lookAhead $ choice [wordBreak, try $ void wordEndTag])
  ends <- many $ wordEndTag
  return (starts ++ wPiece:wordPieces ++ ends)

document = do
  option () $ void whitespace
  paragraphs <- paragraph `sepBy` paragraphBreak
  option () $ void whitespace
  return paragraphs

paragraphBreak = do
  string "\n\n"
  return ()

paragraph = word `sepBy` whitespace >>= (return . Paragraph . intercalate [Whitespace])

findOpeningTag' :: [Chunk] -> Integer -> [Chunk] -> Chunk -> Maybe [Chunk]
findOpeningTag' processedChunks skip [] tag = Nothing
findOpeningTag' processedChunks skip (lastChunk:chunks) tag =
  if closingTag lastChunk == Just tag then
    if skip > 0 then
      findOpeningTag' (lastChunk:processedChunks) (skip-1) chunks tag 
      else
      Just (reverse processedChunks ++ (lastChunk:chunks))
    else if lastChunk == tag then
      findOpeningTag' (lastChunk:processedChunks) (skip+1) chunks tag 
    else if isJust (closingTag lastChunk) then
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
  if closingTag lastOpen == Just nextChunk then
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
  input <- getContents
  let rl = parse document "(unknown)" $ T.strip $ T.pack input
  print rl
  case rl of
    Right text -> putStrLn $ render text
    Left err -> print err