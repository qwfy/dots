#!/usr/bin/env runhaskell

import System.Environment
import System.FilePath
import System.Directory
import Control.Monad (liftM)

repoPath = "~/codes/dots"
-- path:  (osLocation                    , repoLocation)
files = [ ("~/.vimrc"                    , "$repoPath/vimrc")
        , ("~/.xinitrc"                  , "$repoPath/xinitrc")
        , ("~/.xmodmap"                  , "$repoPath/xmodmap")
        , ("~/.wgetrc"                   , "$repoPath/wgetrc")
        , ("~/.ghci"                     , "$repoPath/ghci")
        , ("~/bin/pac.js"                , "$repoPath/pac.js")
        , ("~/bin/bashrc"                , "$repoPath/bashrc")
        , ("~/.vim/snippets/erlang.snip" , "$repoPath/erlang.snip")
        ]

expandPath :: FilePath -> IO FilePath
expandPath p =
    let paths = splitDirectories p
    in liftM joinPath $ mapM expandOne paths
    where expandOne :: FilePath -> IO FilePath
          expandOne "~" = getEnv "HOME"
          expandOne "$repoPath" = expandPath repoPath
          expandOne ('$' : var) = getEnv var
          expandOne x = return x

data Direction = Os2Repo | Repo2Os

instance (Show Direction) where
    show Os2Repo = "->"
    show Repo2Os = "<-"

copyTuple :: Direction -> (FilePath, FilePath) -> IO ()
copyTuple d@Os2Repo (x, y) = do
    x' <- expandPath x
    y' <- expandPath y
    putStrLn . concat $ [x', surround . show $ d, y']
    copyFile x' y'
copyTuple Repo2Os (x, y) = copyTuple Os2Repo (y, x)

surround :: String -> String
surround x = concat ["\t", x, " "]

main :: IO ()
main = do
    [direction] <- getArgs
    let direction' = case direction of
                         "os2repo" -> Os2Repo
                         "repo2os" -> Repo2Os
    mapM_ (copyTuple direction') files
