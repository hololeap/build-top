{-# Language FlexibleContexts #-}

module BuildTop.Util where

import Control.Applicative (Alternative, empty)
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Maybe
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.FilePath
import UnliftIO.Async (Async, async)

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

-- | Lift a 'Maybe' value into an 'Alternative'
liftMaybe :: Alternative f => Maybe a -> f a
liftMaybe (Just x) = pure x
liftMaybe Nothing = empty

-- | Convert a 'MaybeT' function into a function suitable for @alter@.
alterMaybeT :: Monad m => (a -> MaybeT m b) -> Maybe a -> m (Maybe b)
alterMaybeT f = runMaybeT . (f <=< liftMaybe)

-- | Run an IO action indefinitely in another thread
foreverThread :: MonadUnliftIO m => m a -> m (Async b)
foreverThread = async . forever
