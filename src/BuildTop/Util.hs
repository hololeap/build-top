{-# Language FlexibleContexts #-}

module BuildTop.Util where

import Control.Monad.Trans.Maybe
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.FilePath

import Data.Parsable hiding ((<|>))
import Distribution.Portage.Types

lockFilePackage :: Category -> FilePath -> Maybe Package
lockFilePackage c p0 = do
    ('.' : rest) <- pure p0
    (p, ".lockfile") <- pure $ splitExtension rest
    parseMaybe (toString c </> p)

decodeString :: ByteString -> String
decodeString = T.unpack . T.decodeUtf8

parseMaybe :: Parsable a Identity () String => String -> Maybe a
parseMaybe = either (const Nothing) Just . runParsable ""

-- | Lift a 'Maybe' value into a 'MaybeT'
liftMaybe :: Applicative m => Maybe a -> MaybeT m a
liftMaybe = MaybeT . pure

-- | Convert a 'MaybeT' function into a function suitable for @alter@.
alterMaybeT :: Monad m => (a -> MaybeT m b) -> Maybe a -> m (Maybe b)
alterMaybeT f = runMaybeT . (f <=< liftMaybe)
