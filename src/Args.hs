module Args where

import Data.List
import Data.Maybe
import Result
import Text.Read (readMaybe)

data Args = Args
  { argImageConfigFile :: Maybe String, -- ^ Given with the `-imageConfigFile` argument
    argSceneFile :: Maybe String, -- ^ Given with the `-sceneFile` argument
    argOutFile :: Maybe String, -- ^ Given with the `-outFile` argument
    argNrSamples :: Maybe Int -- ^ Given with the `-imageNrSamples` argument
  }
  deriving (Eq, Show)

data ParseArgsError = InvalidArgs deriving (Eq, Show)

type ArgMap = [(String, String)]

-- >>> toArgMap ["-x", "y"]
-- Success [("x","y")]
--
-- >>> toArgMap ["-x", "y", "-a", "b"]
-- Success [("x","y"),("a","b")]
--
-- >>> toArgMap ["x", "y"]
-- Error InvalidArgs
--
-- >>> toArgMap ["-x", "y", "-z"]
-- Error InvalidArgs
toArgMap :: [String] -> Result ParseArgsError ArgMap
-- It takes a list of arguments of the form
--["-key1", "value1", "-key2", "value2", ...] and converts it to a list of pairs:
--[("key1", "value1"), ("key2", "value2")] .
-- If the list of arguments is not of the correct form, it returns an error.
toArgMap args =
    if null args then Success [] else
    let argMap = parseArgs args
    in case argMap of
        Left error -> Error error
        Right result -> Success result
    where
        parseArgs :: [String] -> Either ParseArgsError ArgMap
        parseArgs [] = Right []
        parseArgs (x:xs) =
            case x of
                '-' : key ->
                    case xs of
                        [] -> Left InvalidArgs
                        (value:rest) ->
                            case parseArgs rest of
                                Left error -> Left error
                                Right result -> Right ((key, value):result)
                _ -> Left InvalidArgs


-- >>> getArg "key" [("key", "value")]
-- Just "value"
getArg :: String -> ArgMap -> Maybe String
getArg = lookup

-- >>> readArg "name" [("name", "1")] :: Maybe Int
-- Just 1
--
-- >>> readArg "name" [("name", "one")] :: Maybe Int
-- Nothing
--
-- >>> readArg "number" [("name", "1")] :: Maybe Int
-- Nothing
readArg :: (Read a) => String -> ArgMap -> Maybe a
readArg key argMap = do
  value <- lookup key argMap
  readMaybe value

toArgMapFunc :: Result ParseArgsError ArgMap -> ArgMap
toArgMapFunc args =
  case args of
    (Success x) -> x
    _ -> [("empty", "em")]

-- >>> procArgs ["-imageNrSamples", "200", "-outFile", "image.bmp"]
-- Success (Args {argImageConfigFile = Nothing, argSceneFile = Nothing, argOutFile = Just "image.bmp", argNrSamples = Just 200})
procArgs :: [String] -> Result ParseArgsError Args
procArgs args =
  let argMap = toArgMap args
      argMapFunc = toArgMapFunc argMap
   in case argMap of
        Error InvalidArgs -> Error InvalidArgs
        Success _ ->
          Success
            Args
              { argImageConfigFile = getArg "imageConfigFile" argMapFunc,
                argSceneFile = getArg "sceneFile" argMapFunc,
                argOutFile = getArg "outFile" argMapFunc,
                argNrSamples = readArg "imageNrSamples" argMapFunc
              }