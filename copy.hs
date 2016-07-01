#!/usr/bin/env runhaskell

import System.Environment
import System.FilePath
import System.Directory
import Control.Monad (liftM)
import Control.Applicative ((<$>))

repoPath = "~/codes/dots"
-- path:  (osLocation                             , repoLocation)
files = [ ("~/.vimrc"                             , "$repoPath/vimrc")
        , ("~/.xinitrc"                           , "$repoPath/xinitrc")
        , ("~/.xmodmap"                           , "$repoPath/xmodmap")
        , ("~/.wgetrc"                            , "$repoPath/wgetrc")
        , ("~/.gitconfig"                         , "$repoPath/gitconfig")
        , ("~/.ghci"                              , "$repoPath/ghci")
        , ("~/bin/pac.js"                         , "$repoPath/pac.js")
        , ("~/.bashrc"                            , "$repoPath/bashrc")
        , ("~/.vim/snippets/erlang.snip"          , "$repoPath/erlang.snip")
        , ("~/.vim/snippets/haskell.snip"         , "$repoPath/haskell.snip")
        , ("~/.local/share/konsole/Shell.profile" , "$repoPath/Shell.profile")
        , ("~/.config/octave/qt-settings"         , "$repoPath/octave.qt-settings")
        , ("~/.octaverc"                          , "$repoPath/octaverc")
        ]

expandPath :: FilePath -> IO FilePath
expandPath p =
    let paths = splitDirectories p
    in joinPath <$> mapM expandOne paths
    where expandOne :: FilePath -> IO FilePath
          expandOne "~" = getEnv "HOME"
          expandOne "$repoPath" = expandPath repoPath
          expandOne ('$' : var) = getEnv var
          expandOne x = return x

makeCopy :: FilePath -> FilePath -> IO ()
makeCopy x y = do
    x' <- expandPath x
    y' <- expandPath y
    createDirectoryIfMissing True $ takeDirectory y'
    putStrLn . concat $ [x', " -> " , y']
    copyFile x' y'

main :: IO ()
main = do
    [direction] <- getArgs
    let modifier = case direction of
                       "os2repo" -> id
                       "repo2os" -> flip
    mapM_ (uncurry $ modifier makeCopy) files
