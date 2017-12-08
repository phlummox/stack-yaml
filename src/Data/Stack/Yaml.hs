{-# LANGUAGE OverloadedStrings #-}

{- |

Parse a stack.yaml file

  *   credit: stack2nix v. 0.1.2.0

      https://hackage.haskell.org/package/stack2nix-0.1.2.0/docs/Stack2nix.html 

      author: Jacob Mitchell (jacob.mitchell@iohk.io)

-}
module Data.Stack.Yaml
  (
    Package(..)
  , RemotePkgConf(..)
  , StackConfig(..)
  , parseStackYaml
  , getResolver
  ) where

import           Control.Monad                (unless)
import qualified Data.ByteString              as BS
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text, unpack)
import           Data.Yaml                    (FromJSON (..), (.!=), (.:),
                                               (.:?))
import qualified Data.Yaml                    as Y
import           System.Directory             (doesFileExist)
import           System.FilePath              ((</>))



data StackConfig = StackConfig
  { resolver  :: Text
  , packages  :: [Package]
  , extraDeps :: [Text]
  }
  deriving (Show, Eq)

data Package = LocalPkg FilePath
             | RemotePkg RemotePkgConf
             deriving (Show, Eq)

data RemotePkgConf = RemotePkgConf
  { gitUrl   :: Text
  , commit   :: Text
  , extraDep :: Bool
  }
  deriving (Show, Eq)

instance FromJSON StackConfig where
  parseJSON (Y.Object v) =
    StackConfig <$>
    v .: "resolver" <*>
    v .: "packages" <*>
    v .: "extra-deps"
  parseJSON _ = fail "Expected Object for StackConfig value"

instance FromJSON Package where
  parseJSON (Y.String v) = return $ LocalPkg $ unpack v
  parseJSON obj@(Y.Object _) = RemotePkg <$> parseJSON obj
  parseJSON _ = fail "Expected String or Object for Package value"

instance FromJSON RemotePkgConf where
  parseJSON (Y.Object v) = do
    loc <- v .: "location"
    gitUrl <- loc .: "git"
    commit <- loc .: "commit"
    extra <- v .:? "extra-dep" .!= False
    return $ RemotePkgConf gitUrl commit extra
  parseJSON _ = fail "Expected Object for RemotePkgConf value"

-- | parse a stack.yaml file.
--
-- example: 
--
-- >  import qualified Data.ByteString as BS
-- >  maybeConfig <- parseStackYaml <$> BS.readFile "stack.yaml"
-- 
-- or:
-- 
-- >>> import qualified Data.ByteString.Char8 as BSC
-- >>> import Data.Maybe (fromJust)
-- >>> 'a'
-- 'a'
-- >>> let yaml = BSC.pack $ unlines [ "resolver: lts-8.24" , "packages:" , "- ." , "extra-deps: []" , "flags: {}" , "extra-package-dbs: []" ] 
-- >>> let config = fromJust $ parseStackYaml yaml
-- >>> config
-- StackConfig {resolver = "lts-8.24", packages = [LocalPkg "."], extraDeps = []}
--
parseStackYaml :: BS.ByteString -> Maybe StackConfig
parseStackYaml = Y.decode

-- | Look for a stack.yaml file in the working directory,
--  extract the 'resolver:' field, and return it as Text.
getResolver :: IO Text
getResolver = do
  let curDir = "."
  let fname = curDir </> "stack.yaml"
  doesFileExist fname >>= 
    flip unless (error $ "no such file " <> fname) 
  contents <- BS.readFile fname
  case parseStackYaml contents of
    Just config -> return (resolver config)
    Nothing     -> error $ "Failed to parse " <> fname



