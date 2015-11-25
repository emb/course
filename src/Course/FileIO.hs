{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Monad
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: Chars -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

$ runhaskell FileIO.hs "files.txt"
============ a.txt
the contents of a

============ b.txt
the contents of b

============ c.txt
the contents of c

-}

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main = main' =<< getArgs
       where
         main' (fname :. Nil) = run fname
         main' _              =
           putStrLn "[ERROR]\nUsage: runhaskell Course/FileIO \"FILE_NAME\""

type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  Chars
  -> IO ()
run fname = readFile fname >>= getFiles' >>= printFiles
            where
              getFiles' = getFiles . lines

getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles =
  sequence . (<$>) getFile
  --sequence (getFile <$> files)

getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile file =
  (\content -> (file, content)) <$> readFile file

printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles =
  void . sequence . (<$>) (\(a,b) -> printFile a b)
  ---- ^^ using composition and removing files as the argument.
  --void (sequence ((<$>) (\(a,b) -> printFile a b) files))
  --void (sequence ((\(a,b) -> printFile a b) <$> files))


printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile name content =
  putStrLn ("========== " ++ name) *> putStrLn content
