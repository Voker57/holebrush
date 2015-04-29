{- 

% ./test <<< "_______________________________________huita________________________________________hui"                                                                                                                           
<em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em><em>huita________________________________________hui
</em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em></em>

-}

module Main where

import Data.Maybe
import Data.Char
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Data.List


data Paragraph = Paragraph [Chunk]

data Chunk = ItalicStart | ItalicEnd | BoldStart | BoldEnd | Plaintext String deriving (Eq, Show)

isClosingTag :: Chunk -> Bool
isClosingTag t = case t of
  ItalicEnd -> True
  BoldEnd -> True
  _ -> False

isOpeningTag :: Chunk -> Bool
isOpeningTag = isJust . closingTag

closingTag :: Chunk -> Maybe Chunk
closingTag t = case t of
  ItalicStart -> Just ItalicEnd
  BoldStart -> Just BoldEnd
  _ -> Nothing

toPlainText :: Chunk -> String
toPlainText t = case t of
  ItalicStart -> "_"
  ItalicEnd -> "_"
  BoldStart -> "*"
  BoldEnd -> "*"
  Plaintext str -> str

data OpenedTag = Italic | Bold deriving (Eq, Show)

italicStart = do
  (char '_') 
  notFollowedBy space
  return ItalicStart

boldStart = do
  (char '*')
  notFollowedBy space
  return BoldStart

italicEnd = do
  (char '_')
  lookAhead $ try (choice [italicEnd >> return (), boldEnd >> return (), space >> return (), eof])
  return ItalicEnd

boldEnd = do
  (char '*')
  lookAhead $ try (choice [italicEnd >> return (), boldEnd >> return (), space >> return (), eof])
  return BoldEnd

whitespace = do
  scs <- many1 space
  return (Plaintext scs)

word = do
  sc <- anyChar
  scs <- manyTill anyChar (lookAhead $ try $ choice [italicEnd >> return (), boldEnd >> return (), whitespace >> return (), eof])
  return (Plaintext (sc:scs))

documentPiece = do
  starts <- many $ try $ choice [italicStart, boldStart]
  theWord <- word
  ends <- many $ choice [italicEnd, boldEnd]
  theSpace <- option (Plaintext "") whitespace
  return (starts ++ [theWord] ++ ends ++ [theSpace])

document = do
  initialSpace <- option (undefined) (lineBreakingWhiteSpace)
  paragraphs <- paragraph `sepEndBy` lineBreakingWhiteSpace
  return paragraphs

lineBreakingWhiteSpace = do
  many (satisfy (\c -> isSpace c && not (c== '\n')))
  char '\n'
  many space
  return ()

paragraph = manyTill documentPiece (lookAhead $ choice [string "\n\n" >> return (), eof])

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

folder (openedTags, text) ItalicStart = (Italic:openedTags, text ++ "<em>")
folder (openedTags, text) BoldStart = (Bold:openedTags, text ++ "<strong>")
folder (Italic:openedTags, text) ItalicEnd = (openedTags, text ++ "</em>")
folder (openedTags, text) ItalicEnd = (openedTags, text ++ "_")
folder (Bold:openedTags, text) BoldEnd = (openedTags, text ++ "</strong>")
folder (openedTags, text) BoldEnd = (openedTags, text ++ "*")
folder (openedTags, text) (Plaintext str) = (openedTags, text ++ str)

main = do
  input <- getContents
  let (Right r) = parse document "(unknown)" input
  undefined
--   let initialState = ([], "")
--   let (strayTags, text) = foldl' folder initialState r
--   let finalText = text ++ (concat $ map (\a -> case a of Italic -> "</em>"; Bold -> "</strong>") $ reverse strayTags)
--   putStr finalText