#!/usr/bin/env runhaskell

import System.Environment
import System.FilePath
import System.Directory
import Control.Monad (liftM)
import Control.Applicative ((<$>))

repoPath = "~/project/dots"
-- path:  (osLocation                               , repoLocation)
files =   [ ("~/.vimrc"                             , "$repoPath/vimrc")
          , ("~/.config/nvim/init.vim"              , "$repoPath/init.vim")
          , ("~/.config/nvim/ginit.vim"             , "$repoPath/ginit.vim")
          , ("~/.xinitrc"                           , "$repoPath/xinitrc")
          , ("~/.inputrc"                           , "$repoPath/inputrc")
          , ("~/.wgetrc"                            , "$repoPath/wgetrc")
          , ("~/.gitconfig"                         , "$repoPath/gitconfig")
          , ("~/.gitignore"                         , "$repoPath/gitignore")
          , ("~/.ghci"                              , "$repoPath/ghci")
          , ("~/bin/pac.js"                         , "$repoPath/pac.js")
          , ("~/.bashrc"                            , "$repoPath/bashrc")
          , ("~/.bash_profile"                      , "$repoPath/bash_profile")
          , ("~/.config/nvim/snippets/erlang.snip"  , "$repoPath/erlang.snip")
          , ("~/.config/nvim/snippets/haskell.snip" , "$repoPath/haskell.snip")
          , ("~/.ssh/config"                        , "$repoPath/ssh_config")
          , ("~/.ocamlinit"                         , "$repoPath/ocamlinit")
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
