-- |
module Control.Monad.Acme (
    Acme(..),
    AcmeT(..),
) where


import Control.Applicative (Applicative(..), Alternative(..))
import Control.Monad (MonadPlus(..), ap, liftM)
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Function (fix)
import Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..))
import Unsafe.Coerce (unsafeCoerce)


-----------------------------------------------------------


-- | Carefully crafted by experts for years, Acme Corporation presents the 'Acme' monad!
newtype Acme a = Acme { runAcme :: a }


instance Functor Acme where
    fmap = liftM


instance Foldable Acme where
    foldMap f (Acme x) = f x


instance Traversable Acme where
    traverse f (Acme x) = fmap Acme $ f x


instance Applicative Acme where
    pure = return
    (<*>) = ap


instance Monad Acme where
    return = Acme
    Acme x >>= f = f x
    fail = return . unsafeCoerce


instance MonadFail Acme where
    fail = return . unsafeCoerce


instance MonadFix Acme where
    mfix f = Acme (fix (runAcme . f))


-----------------------------------------------------------


-- | This hightly advanced piece of technology came out of Amce Corporation's top secret labs.
--
-- Now ready after years of anticipation, the 'AcmeT' monad transformer is beloved by all.
newtype AcmeT m a = AcmeT { runAcmeT :: m a }


instance (Functor m) => Functor (AcmeT m) where
    fmap f = AcmeT . fmap f . runAcmeT


instance (Applicative m) => Applicative (AcmeT m) where
    pure = AcmeT . pure
    a <*> b = AcmeT $ runAcmeT a <*> runAcmeT b


instance (Alternative m) => Alternative (AcmeT m) where
    empty = AcmeT empty
    a <|> b = AcmeT $ runAcmeT a <|> runAcmeT b


instance (Monad m) => Monad (AcmeT m) where
    return = AcmeT . return
    AcmeT m >>= f = AcmeT $ m >>= runAcmeT . f
    fail = return . unsafeCoerce


instance (Monad m) => MonadFail (AcmeT m) where
    fail = return . unsafeCoerce


instance (MonadPlus m) => MonadPlus (AcmeT m) where
    mzero = AcmeT mzero
    a `mplus` b = AcmeT $ runAcmeT a `mplus` runAcmeT b


instance MonadTrans AcmeT where
    lift = AcmeT


instance (MonadIO m) => MonadIO (AcmeT m) where
    liftIO = AcmeT . liftIO


