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
          | Letter { unLetter :: Char }
          | Escaped Char
          
            deriving Show

---------------------------------------------------------------------------

emits :: Document -> String
emits its = do it <- its ; emit it

emit :: Item -> String
emit Newline = "\n"
emit (Comment _) = "%\n"
emit (Command name opt arg) = "\\" ++ name
  ++ (case opt of Nothing -> "" ; Just o -> emit o ) ++ emit arg
emit (Verbatim s) = "\\begin{verbatim}" ++ s ++ "\\end{verbatim}"
emit (Braced doc) = "{" ++ emits doc ++ "}"
emit (Bracketed doc) = "[" ++ emits doc ++ "]"
emit (Letter c) = [c]
emit (Escaped c) = [ '\\', c]

handles :: Mode -> FilePath -> Document -> IO ()
handles mode top its = mapM_ (handle mode top) its

handle :: Mode -> FilePath -> Item -> IO ()
handle mode top (Command "input" Nothing (Braced doc)) = fhandle mode ".tex" $ map unLetter doc
handle mode top (Command "inputt" Nothing (Braced doc)) = fhandle mode ".tex" $ map unLetter doc
handle mode top it @ (Command "bibliography" Nothing _) = case mode of
  Expand -> fhandle Expand ".bbl" top
  Keep -> putStr $ emit it
handle mode top (Command _ _ _) = return () -- ignore TODO, etc.
handle mode top it = putStr $ emit it

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
  [ "inputt", "input"  , "bibliography"
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
