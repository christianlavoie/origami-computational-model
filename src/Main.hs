module Main where

import Control.Lens

import Control.Monad (when)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import Text.Printf (printf)

import qualified System.Console.GetOpt as GetOpt

import Parser
import Tokenizer
import Types


-- Options parsing
data Options = Options {
    optInput :: Maybe FilePath,
    optVerbose :: Bool }
  deriving Show

defaultOptions :: Options
defaultOptions = Options {
    optInput = Nothing,
    optVerbose = False }

options :: [GetOpt.OptDescr (Options -> Options)]
options = [
    GetOpt.Option ['v'] ["verbose"]
        (GetOpt.NoArg (\opts -> opts { optVerbose = True }))
        "chatty output on stderr",

    GetOpt.Option ['i'] ["input-file"]
        (GetOpt.OptArg ((\f opts -> opts { optInput = Just f }) . fromMaybe "input") "FILE")
        "input FILE" ]

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
   case GetOpt.getOpt GetOpt.Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> ioError (userError (concat errs ++ GetOpt.usageInfo header options))
  where header = "Usage: ic [OPTION...] files..."

-- Actual program
main :: IO ()
main = do
    content <- getContents

    -- Parse args
    args <- getArgs
    (opts, leftover) <- compilerOpts args

    case leftover of
        [] -> return ()
        _ -> error $ "Unknown args " ++ show leftover

    -- Tokenize
    tokenized <- case tokenize "(unknown)" content of
        Left te -> error $ show te
        Right tokenized_ -> return tokenized_

    when (optVerbose opts) $ do
        putStrLn "Tokenized:"
        mapM_ putStr [ printf "\t%s\n" (show toks) | toks <- tokenized ]
        putStrLn ""

    -- Parse
    parsed <- case parseOCM tokenized of
        Left pe -> error $ show pe
        Right parsed_ -> return parsed_

    when (optVerbose opts) $ do
        putStrLn "Parsed:"
        putStrLn $ show parsed
