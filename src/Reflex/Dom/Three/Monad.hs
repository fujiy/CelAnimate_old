{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module Reflex.Dom.Three.Monad
    ( MonadJSM
    , JSVal
    , ToJSVal
    , lift
    , liftJSM

    , ObjectBuilderT(..)
    , ObjectBuilderEnv(..)
    , ObjectTimeline
    , Object3D
    , three
    , newThree
    , addParent
    ) where

import           Control.Lens
import           Control.Monad.Reader
import qualified Data.Map                                     as M
import           Data.Text                                    (Text)
import           JSDOM.Types                                  hiding (Text)
import           Language.Javascript.JSaddle.Classes.Internal
import           Language.Javascript.JSaddle.Object
import           Reflex.Spider

type Three = JSM

newtype ObjectBuilderT t m a = ObjectBuilderT
    { unObjectBuilderT :: ReaderT (ObjectBuilderEnv t) m a }
    deriving ( Functor, Applicative, Monad, MonadFix
             , MonadReader (ObjectBuilderEnv t) )

instance MonadTrans (ObjectBuilderT t) where
    lift = ObjectBuilderT . lift

instance MonadIO m => MonadIO (ObjectBuilderT t m) where
    liftIO = lift . liftIO

instance MonadJSM m => MonadJSM (ObjectBuilderT t m) where

data ObjectBuilderEnv t = ObjectBuilderEnv
    { _parent :: JSVal }

makeLenses 'ObjectBuilderEnv

type ObjectTimeline = Spider

type Object3D = ObjectBuilderT ObjectTimeline JSM

three :: JSM JSVal
three = jsg "THREE"

newThree :: MakeArgs args => Text -> args -> JSM JSVal
newThree n = new (three ! n)

addParent :: (MonadJSM m, ToJSVal a) => a -> ObjectBuilderT t m ()
addParent x = do
    p <- view parent
    liftJSM $ p ^. js1 "add" x
    return ()


