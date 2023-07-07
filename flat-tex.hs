-- | flat-tex

{-
will read main.tex, will write to stdout
recursively inline all \inputs ,
delete all comments ( % )
and remove all \todo{ } etc. (see "known_commands" below)

compile:  cabal install,
use: flat-tex main
or: flat-tex --keep-bib main

(C) J. Waldmann , License: GPL
-}

module Main where

import Text.ParserCombinators.Parsec
import System.Environment ( getArgs )
import System.Directory
import Control.Monad (void)
import System.IO

main :: IO ()
main = do
    argv <- getArgs
    case argv of
      "--keep-bib" : fnames ->
        mapM_ (fhandle Keep ".tex") fnames
      fnames ->
        mapM_ (fhandle Expand ".tex") fnames

type Document = [ Item ]

data Item = Newline
          | Comment String
          | Command String (Maybe Item) Item
          -- ^ with optional argument in [], exactly one argument in braces
          | Braced Document
          | Bracketed Document
          | Verbatim String -- ^ special: keep % (it is not a comment)
          | VerbatimCap String -- ^ special: keep % (it is not a comment)
          | Verb String -- stop issues with non matching brackets
          | Pagebreak
          | Letter { unLetter :: Char }
          | Escaped Char
          
            deriving Show

---------------------------------------------------------------------------

emits :: Document -> String
emits = concatMap emit

emit :: Item -> String
emit Newline = "\n"
emit (Comment cs) = "%"
emit (Command name opt arg) = "\\" ++ name
  ++ (case opt of Nothing -> "" ; Just o -> emit o ) ++ emit arg
emit (Verbatim s) = "\\begin{verbatim}" ++ s ++ "\\end{verbatim}"
emit (VerbatimCap s) = "\\begin{Verbatim}" ++ s ++ "\\end{Verbatim}"
emit (Verb s) = "\\verb|" ++ s ++ "|"
emit (Pagebreak) = "\\pagebreak{}\n"
emit (Braced doc) = "{" ++ emits doc ++ "}"
emit (Bracketed doc) = "[" ++ emits doc ++ "]"
emit (Letter c) = [c]
emit (Escaped c) = [ '\\', c]

isSpace (Letter ' ') = True
isSpace _ = False

handles :: Mode -> FilePath -> Document -> IO ()
handles mode top [] = return ()
handles mode top (it:its) = case it of
  (Command "include" Nothing (Braced doc)) -> do
    putStr $ emit (Pagebreak)
    fhandle mode ".tex" $ map unLetter doc
    handles mode top its
  (Command "input" Nothing (Braced doc)) -> do
    fhandle mode ".tex" $ map unLetter doc
    handles mode top its
  (Command "inputt" Nothing (Braced doc)) -> do
    fhandle mode ".tex" $ map unLetter doc
    handles mode top its
  (Command "bibliography" Nothing _) -> do
    case mode of
      Expand -> fhandle Expand ".bbl" top
      Keep -> putStr $ emit it
    handles mode top its
  (Command _ _ _) -> do -- ignore TODO, etc.
    case dropWhile isSpace its of
      Newline : more ->do
        putStr $ emit (Comment undefined)
        handles mode top more
      more -> do
        handles mode top more
  _ -> do
    putStr $ emit it
    handles mode top its

data Mode = Keep | Expand

fhandle :: Mode -> String -> FilePath -> IO ()
fhandle mode extension fname = do
    e <- doesFileExist fname
    let actual_fname = case e of
          True -> fname
          False -> fname ++ extension
    p <- parseFromFile (document <* eof) actual_fname
    case p of
           Right doc -> handles mode fname doc
           Left e -> do
             s <- readFile actual_fname
             let pos = errorPos e
                 l = sourceLine pos - 1
                 c = sourceColumn pos - 1
                 (lpre, lthis : lpost) = splitAt l $ lines s
                 (cpre, cthis : cpost) = splitAt c $ lthis
                 underline = replicate c '.'
                   ++ replicate (length lthis - c) '^'
                 context = 2
             error $ unlines $ [ show e ] ++
               ekat context lpre ++ [ lthis, underline ] ++ take context lpost
               

ekat k = reverse . take k . reverse

---------------------------------------------------------------------------

known_commands :: [ String ]
known_commands =
  [ "include", "inputt", "input"  , "bibliography"
  , "todo", "reminder", "ignore", "done"
  ]

document :: Parser Document
document = many item

item :: Parser Item
item =   do newline
            return Newline
     <|> do char '%'
            cs <- anyChar `manyTill` (void newline <|> eof)
            return $ Comment cs
     <|> do try (string "\\verb|")
            cs <- anyChar `manyTill` try (string "|")
            return $ Verb cs
     <|> do try (string "\\begin{Verbatim}")
            cs <- anyChar `manyTill` try (string "\\end{Verbatim}")
            return $ VerbatimCap cs
     <|> do try (string "\\begin{verbatim}")
            cs <- anyChar `manyTill` try (string "\\end{verbatim}")
            return $ Verbatim cs
     <|> do char '\\'
            command known_commands <|> Escaped <$> anyChar
     <|> do braced
     <|> do bracketed
     <|> do c <- satisfy ( \ c -> not $ elem c "}]" ) ; return $ Letter c

braced :: Parser Item
braced = Braced <$> between ( char '{' ) ( char '}' ) document

bracketed :: Parser Item
bracketed = Bracketed <$> between ( char '[' ) ( char ']' ) document

command :: [ String ] -> Parser Item
command names = try $ 
  Command <$> ( choice $ map ( try . string ) names )
          <*> optionMaybe bracketed
          <*> braced

----------------------------------

test :: IO ()
test = fhandle Expand ".tex" "test"
