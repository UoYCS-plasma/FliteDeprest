module Flite.Flite (main) where

import Flite.Desugar
import Flite.Syntax
import Flite.ParseLib
import Flite.Parse
import Flite.Pretty
import Flite.Interp
import Flite.Inline
import Flite.Compile
--import Flite.RedCompile
import Flite.CantileverCompile
import Flite.TypeChecker2
import Flite.Flic
import qualified Flite.RedFrontend
import Data.List
import System.Environment
import System.IO
import System.Console.GetOpt

data Flag =
    Desugar
  | CompileToC
  | CompileToCantilever
  | Defunctionalise
  | CompileToRed Int Int Int Int Int
  | Inline (Maybe Int)
  | StrictnessAnalysis
  | TypeCheck
  | CompileType
  | Flic 
  deriving Eq

isDisjoint (Inline i) = False
isDisjoint StrictnessAnalysis = False
isDisjoint flag = True

options :: [OptDescr Flag]
options =
  [ Option ['d'] [] (NoArg Desugar) "desugar"
  , Option ['c'] [] (NoArg CompileToC) "compile to C"
  , Option ['C'] [] (NoArg CompileToCantilever) "compile to Cantilever"
  , Option ['f'] [] (NoArg Defunctionalise) "make first-order"
  , Option ['r'] [] (OptArg red "MAXPUSH:APSIZE:MAXAPS:MAXLUTS:MAXREGS")
                    "compile to Reduceron templates"
  , Option ['i'] [] (OptArg (Inline . fmap read) "MAXAPS")
                    "inline small function bodies"
  , Option ['s'] [] (NoArg StrictnessAnalysis) "employ strictness analysis"
  , Option ['t'] [] (NoArg TypeCheck) "type-checking"
  , Option ['R'] [] (NoArg CompileType) "compile to Reduceron + type-info"
  , Option ['T'] [] (NoArg Flic) "Translate Flite to SPJ Core"
  ]
  where
    redDefaults = CompileToRed 6 4 2 1 0
    red Nothing = redDefaults
    red (Just s) =
      case split ':' s of
        [a, b, c, d, e] ->
          CompileToRed (read a) (read b) (read c) (read d) (read e)
        _ -> error (usageInfo header options)

header = "Usage: Flite [OPTION...] FILE.hs"

main =
  do args <- getArgs
     case getOpt Permute options args of
       (flags, [fileName], []) -> run flags fileName
       (_, _, errs) -> error (concat errs ++ usageInfo header options)

run flags fileName =
  do contents <- readFile fileName
     let p = parse prog contents
     let inlineFlag = head $ [InlineAll | Inline Nothing <- flags]
                          ++ [InlineSmall i | Inline (Just i) <- flags]
                          ++ [NoInline]
     case filter isDisjoint flags of
       [] -> interp inlineFlag p `seq` return ()
       [Desugar] ->
         -- putStrLn $ pretty $ desugar inlineFlag p
         putStrLn $ prettyProg $ desugar inlineFlag p
       [CompileToC] -> putStrLn $ compile inlineFlag p
       [CompileToCantilever] -> putStrLn $ canteleverCompile inlineFlag p
       [Defunctionalise] -> putStrLn $ prettyProg p
{-       [CompileToRed slen alen napps nluts nregs] ->
        do let sa = StrictnessAnalysis `elem` flags
           mapM_ print $ redCompile inlineFlag sa slen alen napps nluts nregs p -}
       [TypeCheck]     -> print $ tcheck p --putStrLn $ showfuntypes $ tcheck p
       [CompileType]   -> do 
                            --tc <- putStrLn $ showfuntypes $ tcheck p
                            --cr <- mapM_ print $ redCompile p  
                            tc <-print (tcheck p)
                            return ()
       [Flic]         -> putStrLn $ flic p
       _ -> error (usageInfo header options)

-- Auxiliary

split :: Eq a => a -> [a] -> [[a]]
split x xs =
  case elemIndex x xs of
    Nothing -> [xs]
    Just i -> let (first, rest) = splitAt i xs in
                first : split x (dropWhile (== x) rest)
