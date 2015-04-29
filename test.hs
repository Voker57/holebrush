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

class Renderable a where
  render :: a -> String

data Paragraph = Paragraph [Chunk] deriving (Show, Eq)

instance Renderable Paragraph where
  render (Paragraph chunks) = "<p>" ++ (concat $ map (render) chunks) ++ "</p>"

instance Renderable Chunk where
  render ItalicStart = "<i>"
  render ItalicEnd = "</i>"
  render BoldStart = "<b>"
  render BoldEnd = "</b>"
  render (Plaintext s) = s
  render Whitespace = " "

instance (Renderable a) => Renderable [a] where
  render xs = concat $ map (render) xs

data Chunk = ItalicStart | ItalicEnd | BoldStart | BoldEnd | Plaintext String | Whitespace deriving (Eq, Show)

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
  char '_'
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
  scs <- manyTill space (lookAhead $ try $ choice [string "\n\n" >> return (), eof, satisfy (not . isSpace) >> return ()])
  return Whitespace

plainWord = do
  sc <- satisfy (not . isSpace)
  scs <- manyTill (satisfy (not . isSpace)) (lookAhead $ try $ choice [italicEnd >> return (), boldEnd >> return (), space >> return (), eof, paragraphBreak])
  return (Plaintext (sc:scs))

word = do
  starts <- many $ try $ choice [italicStart, boldStart]
  theWord <- plainWord
  ends <- many $ choice [italicEnd, boldEnd]
  return (starts ++ [theWord] ++ ends)

document = do
  paragraphs <- paragraph `sepBy` paragraphBreak
  return paragraphs

paragraphBreak = do
  string "\n\n"
  return ()

paragraph = (word `sepBy` whitespace) >>= (return . Paragraph . intercalate [Whitespace])

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

main = do
  input <- getContents
  let rl = parse document "(unknown)" input
  case rl of
    Right text -> do
      let sanitizedText = map (\(Paragraph pieces) -> Paragraph $ orphanReaper $ widowReaper pieces) text
      putStrLn $ render sanitizedText
    Left err -> print err