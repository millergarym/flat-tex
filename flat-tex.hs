-- | flat-tex

-- will read main.tex, will write to stdout
-- recursively inline all \inputs ,
-- delete all comments ( % )
-- and remove all \todo{ } etc. (see "known_commands" below)

-- compile:  cabal install, use: flat-tex main  

-- (C) J. Waldmann , License: GPL


import Text.ParserCombinators.Parsec
import System.Environment ( getArgs )
import System.Directory
import Control.Monad (void)

main :: IO ()
main = do
    fnames <- getArgs
    mapM_ (fhandle  ".tex") fnames

type Document = [ Item ]

data Item = Newline
	  | Comment String
	  | Command String Document -- ^ with exactly one argument, in braces
	  | Group Document
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
emit (Command name doc) = "\\" ++ name ++ "{" ++ emits doc ++ "}"
emit (Verbatim s) = "\\begin{verbatim}" ++ s ++ "\\end{verbatim}"
emit (Group doc) = "{" ++ emits doc ++ "}"
emit (Letter c) = [c]
emit (Escaped c) = [ '\\', c]

handles :: FilePath -> Document -> IO ()
handles top its = mapM_ (handle top) its

handle :: FilePath -> Item -> IO ()
handle top (Command "input" doc) = fhandle ".tex" $ map unLetter doc
handle top (Command "bibliography" _) = fhandle ".bbl" top
handle top (Command _ _) = return ()
handle top it = putStr $ emit it

fhandle :: String -> FilePath -> IO ()
fhandle extension fname = do
    e <- doesFileExist fname
    let actual_fname = case e of
          True -> fname
          False -> fname ++ extension
    p <- parseFromFile (document <* eof) actual_fname
    case p of
	   Right doc -> handles fname doc
	   Left e -> error $ show e

---------------------------------------------------------------------------

known_commands :: [ String ]
known_commands = [ "input", "todo", "reminder", "ignore", "done", "bibliography" ]

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
	    command known_commands
	      <|> do c <- anyChar ; return $ Escaped c
     <|> do group
     <|> do c <- satisfy ( \ c -> c /= '}' ) ; return $ Letter c

group :: Parser Item
group = do
    contents <- between ( char '{' ) ( char '}' ) document
    return $ Group contents

command :: [ String ] -> Parser Item
command names = try $ do
    name <- choice $ map ( string ) names
    Group c <- group
    return $ Command name c



