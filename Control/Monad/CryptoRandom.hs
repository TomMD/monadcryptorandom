{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts
  , GeneralizedNewtypeDeriving, MultiParamTypeClasses, UndecidableInstances #-}
{-|
  Maintainer: Thomas.DuBuisson@gmail.com
  Stability: beta
  Portability: portable

Much like the "MonadRandom" package ("Control.Monad.Random"), this module
provides plumbing for the CryptoRandomGen generators.

-}

module Control.Monad.CryptoRandom
  ( CRandom(..)
  , CRandomR(..)
  , MonadCRandom(..)
  , MonadCRandomR(..)
  , ContainsGenError(..)
  , CRandT(..)
  , CRand
  , runCRandT
  , evalCRandT
  , runCRand
  , evalCRand
  , newGenCRand
  , liftCRand
  , liftCRandT
  , module Crypto.Random
  ) where

import Control.Applicative
import Control.Arrow (right, left, first)
import Control.Monad (liftM)
import qualified Control.Monad.Catch as C (MonadThrow(..), MonadCatch(..))
import Control.Monad.Cont (MonadCont(..))
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class ()
import Control.Monad.Identity (Identity(..))
import Control.Monad.Reader (ReaderT)
import Control.Monad.RWS.Lazy as Lazy
import Control.Monad.RWS.Strict as Strict
import Control.Monad.State.Lazy as Lazy
import Control.Monad.State.Strict as Strict
import Control.Monad.Writer.Class ()
import Control.Monad.Writer.Lazy as Lazy
import Control.Monad.Writer.Strict as Strict
import Crypto.Random (CryptoRandomGen(..), GenError(..))
import Data.Bits (xor, setBit, shiftR, shiftL, (.&.))
import Data.Int
import Data.List (foldl')
import Data.Word
import Data.Proxy
import Data.Tagged
import qualified Data.ByteString as B

-- |@MonadCRandom m@ represents a monad that can produce
-- random values (or fail with a 'GenError').  It is suggested
-- you use the 'CRandT' transformer in your monad stack.
class (ContainsGenError e, MonadError e m) => MonadCRandom e m where
  getCRandom   :: CRandom a => m a
  getBytes     :: Int -> m B.ByteString
  getBytesWithEntropy :: Int -> B.ByteString -> m B.ByteString
  doReseed :: B.ByteString -> m ()

instance MonadCRandom e m => MonadCRandom e (Lazy.StateT s m) where
  getCRandom = lift getCRandom
  {-# INLINE getCRandom #-}
  getBytes = lift . getBytes
  {-# INLINE getBytes #-}
  getBytesWithEntropy i = lift . getBytesWithEntropy i
  {-# INLINE getBytesWithEntropy #-}
  doReseed = lift . doReseed
  {-# INLINE doReseed #-}

instance MonadCRandom e m => MonadCRandom e (Strict.StateT s m) where
  getCRandom = lift getCRandom
  {-# INLINE getCRandom #-}
  getBytes = lift . getBytes
  {-# INLINE getBytes #-}
  getBytesWithEntropy i = lift . getBytesWithEntropy i
  {-# INLINE getBytesWithEntropy #-}
  doReseed = lift . doReseed
  {-# INLINE doReseed #-}

instance (Monoid w, MonadCRandom e m) => MonadCRandom e (Strict.WriterT w m) where
  getCRandom = lift getCRandom
  {-# INLINE getCRandom #-}
  getBytes = lift . getBytes
  {-# INLINE getBytes #-}
  getBytesWithEntropy i = lift . getBytesWithEntropy i
  {-# INLINE getBytesWithEntropy #-}
  doReseed = lift . doReseed
  {-# INLINE doReseed #-}

instance (Monoid w, MonadCRandom e m) => MonadCRandom e (Lazy.WriterT w m) where
  getCRandom = lift getCRandom
  {-# INLINE getCRandom #-}
  getBytes = lift . getBytes
  {-# INLINE getBytes #-}
  getBytesWithEntropy i = lift . getBytesWithEntropy i
  {-# INLINE getBytesWithEntropy #-}
  doReseed = lift . doReseed
  {-# INLINE doReseed #-}

instance MonadCRandom e m => MonadCRandom e (ReaderT r m) where
  getCRandom = lift getCRandom
  {-# INLINE getCRandom #-}
  getBytes = lift . getBytes
  {-# INLINE getBytes #-}
  getBytesWithEntropy i = lift . getBytesWithEntropy i
  {-# INLINE getBytesWithEntropy #-}
  doReseed = lift . doReseed
  {-# INLINE doReseed #-}

instance (Monoid w, MonadCRandom e m) => MonadCRandom e (Strict.RWST r w s m) where
  getCRandom = lift getCRandom
  {-# INLINE getCRandom #-}
  getBytes = lift . getBytes
  {-# INLINE getBytes #-}
  getBytesWithEntropy i = lift . getBytesWithEntropy i
  {-# INLINE getBytesWithEntropy #-}
  doReseed = lift . doReseed
  {-# INLINE doReseed #-}

instance (Monoid w, MonadCRandom e m) => MonadCRandom e (Lazy.RWST r w s m) where
  getCRandom = lift getCRandom
  {-# INLINE getCRandom #-}
  getBytes = lift . getBytes
  {-# INLINE getBytes #-}
  getBytesWithEntropy i = lift . getBytesWithEntropy i
  {-# INLINE getBytesWithEntropy #-}
  doReseed = lift . doReseed
  {-# INLINE doReseed #-}

newGenCRand :: (CryptoRandomGen g, MonadCRandom GenError m, Functor m) => m g
newGenCRand = go 0
  where
  go 1000 = throwError (GenErrorOther "The generator instance requested by newGenCRand never instantiates.")
  go i = do let p = Proxy
                getTypedGen :: (Functor m, CryptoRandomGen g, MonadCRandom GenError m)
                            => Proxy g -> m (Either GenError g)
                getTypedGen pr = fmap newGen (getBytes $ proxy genSeedLength pr)
            res <- getTypedGen p
            case res of
                Left _  -> go (i+1)
                Right g -> return (g `asProxyTypeOf` p)

class (ContainsGenError e, MonadError e m) => MonadCRandomR e m where
  getCRandomR  :: CRandomR a => (a,a) -> m a

class ContainsGenError e where
  toGenError :: e -> Maybe GenError
  fromGenError :: GenError -> e

instance ContainsGenError GenError where
  toGenError = Just
  fromGenError = id

-- |@CRandom a@ is much like the 'Random' class from the "System.Random" module in the "random" package.
-- The main difference is CRandom builds on "crypto-api"'s 'CryptoRandomGen', so it allows
-- explicit failure.
--
-- @crandomR (low,high) g@ as typically instantiated will generate a value between
-- [low, high] inclusively, swapping the pair if high < low.
--
-- Provided instances for @crandom g@ generates randoms between the bounds and between +/- 2^256
-- for Integer.
--
-- The 'crandomR' function has degraded (theoretically unbounded, probabilistically decent) performance
-- the closer your range size (high - low) is to 2^n (from the top).
class CRandom a where
  crandom   :: (CryptoRandomGen g) => g -> Either GenError (a, g)
  crandoms  :: (CryptoRandomGen g) => g -> [a]
  crandoms g =
    case crandom g of
      Left _       -> []
      Right (a,g') -> a : crandoms g'

class CRandomR a where
  crandomR  :: (CryptoRandomGen g) => (a, a) -> g -> Either GenError (a, g)
  crandomRs :: (CryptoRandomGen g) => (a, a) -> g -> [a]
  crandomRs r g =
    case crandomR r g of
      Left _       -> []
      Right (a,g') -> a : crandomRs r g'

instance CRandomR Integer where
  crandomR = crandomR_Num
  {-# INLINE crandomR #-}

instance CRandom Int where
  crandom = crandomR (minBound, maxBound)
  {-# INLINE crandom #-}

instance CRandomR Int where
  crandomR = crandomR_Num
  {-# INLINE crandomR #-}

instance CRandom Word8 where
  crandom = crandomR (minBound, maxBound)
  {-# INLINE crandom #-}

instance CRandomR Word8 where
  crandomR = crandomR_Num
  {-# INLINE crandomR #-}

instance CRandom Word16 where
  crandom = crandomR (minBound, maxBound)
  {-# INLINE crandom #-}

instance CRandomR Word16 where
  crandomR = crandomR_Num
  {-# INLINE crandomR #-}

instance CRandom Word32 where
  crandom = crandomR (minBound, maxBound)
  {-# INLINE crandom #-}

instance CRandomR Word32 where
  crandomR = crandomR_Num
  {-# INLINE crandomR #-}

instance CRandom Word64 where
  crandom = crandomR (minBound, maxBound)
  {-# INLINE crandom #-}

instance CRandomR Word64 where
  crandomR = crandomR_Num
  {-# INLINE crandomR #-}

instance CRandom Int8 where
  crandom = crandomR (minBound, maxBound)
  {-# INLINE crandom #-}

instance CRandomR Int8 where
  crandomR = crandomR_Num
  {-# INLINE crandomR #-}

instance CRandom Int16 where
  crandom = crandomR (minBound, maxBound)
  {-# INLINE crandom #-}

instance CRandomR Int16 where
  crandomR = crandomR_Num
  {-# INLINE crandomR #-}

instance CRandom Int32 where
  crandom = crandomR (minBound, maxBound)
  {-# INLINE crandom #-}

instance CRandomR Int32 where
  crandomR = crandomR_Num
  {-# INLINE crandomR #-}

instance CRandom Int64 where
  crandom = crandomR (minBound, maxBound)
  {-# INLINE crandom #-}

instance CRandomR Int64 where
  crandomR = crandomR_Num
  {-# INLINE crandomR #-}

instance CRandom Bool where
  crandom g = first (toEnum . fromIntegral) `fmap` crandomR (0::Word8,1) g

crandomR_Num :: (Integral a, CryptoRandomGen g) => (a,a) -> g -> Either GenError (a,g)
crandomR_Num (low, high) g
  | high < low = crandomR_Num  (high,low) g
  | high == low = Right (high, g)
  | otherwise = go g
  where
  mask    = foldl' setBit 0 [0 .. fromIntegral nrBits - 1]
  nrBits  = base2Log range
  range :: Integer
  range   = (fromIntegral high) - (fromIntegral low) + 1
  nrBytes = (nrBits + 7) `div` 8
  go gen =
    let offset = genBytes (fromIntegral nrBytes) gen
    in case offset of
    Left err -> Left err
    Right (bs, g') ->
      let res = fromIntegral low + (bs2i bs .&. mask)
      in if res > fromIntegral high then go g' else Right (fromIntegral res, g')
{-# INLINE crandomR_Num #-}

wrap :: (Monad m, ContainsGenError e) => (g -> Either GenError (a,g)) -> CRandT g e m a
wrap f = CRandT $ do
  g <- get
  case f g of
    Right (a,g') -> put g' >> return a
    Left x -> throwError (fromGenError x)
{-# INLINE wrap #-}

liftCRand :: (g -> Either e (a, g)) -> CRand g e a
liftCRand f = CRandT $ Lazy.StateT $ (\g -> ExceptT $ Identity $ f g)
{-# INLINE liftCRand #-}

liftCRandT :: (Monad m) => (g -> Either e (a, g)) -> CRandT g e m a
liftCRandT f = CRandT $ Lazy.StateT $ (\g -> ExceptT $ return $ f g)
{-# INLINE liftCRandT #-}

-- |CRandT is the transformer suggested for MonadCRandom.
newtype CRandT g e m a = CRandT { unCRandT :: Lazy.StateT g (ExceptT e m) a } 
  deriving (Functor, Applicative, Monad, MonadIO, MonadError e, MonadFix)


instance MonadTrans (CRandT g e) where
  lift = CRandT . lift . lift
  {-# INLINE lift #-}

instance (MonadState s m) => MonadState s (CRandT g e m) where
  get = lift get
  {-# INLINE get #-}
  put = lift . put
  {-# INLINE put #-}

instance (MonadReader r m) => MonadReader r (CRandT g e m) where
  ask = lift ask
  {-# INLINE ask #-}
  local f = CRandT . local f . unCRandT
  {-# INLINE local #-}

instance (MonadWriter w m) => MonadWriter w (CRandT g e m) where
  tell = lift . tell
  {-# INLINE tell #-}
  listen = CRandT . listen . unCRandT
  {-# INLINE listen #-}
  pass = CRandT . pass . unCRandT
  {-# INLINE pass #-}

instance (MonadCont m) => MonadCont (CRandT g e m) where
  callCC f = CRandT $ callCC $ \amb -> unCRandT $ f (CRandT . amb)
  {-# INLINE callCC #-}

-- | Throws exceptions into the base monad.
--
-- @since 0.7.1
instance C.MonadThrow m => C.MonadThrow (CRandT g e m) where
    throwM = CRandT . C.throwM

-- | Catches exceptions from the base monad.
--
-- @since 0.7.1
instance C.MonadCatch m => C.MonadCatch (CRandT g e m) where
  catch (CRandT m) f = CRandT $ C.catch m (unCRandT . f)

-- |Simple users of generators can use CRand for
-- quick and easy generation of randoms.  See
-- below for a simple use of 'newGenIO' (from "crypto-api"),
-- 'getCRandom', 'getBytes', and 'runCRandom'.
--
-- @
-- getRandPair = do
--   int <- getCRandom
--   bytes <- getBytes 100
--   return (int, bytes)
--
-- func = do
--   g <- newGenIO
--   case runCRand getRandPair g of
--     Right ((int,bytes), g') -> useRandomVals (int,bytes)
--     Left x -> handleGenError x
-- @
type CRand g e = CRandT g e Identity

runCRandT :: ContainsGenError e => CRandT g e m a -> g -> m (Either e (a,g))
runCRandT m g = runExceptT . flip Lazy.runStateT g . unCRandT $ m
{-# INLINE runCRandT #-}

evalCRandT :: (ContainsGenError e, Monad m) => CRandT g e m a -> g -> m (Either e a)
evalCRandT m g = liftM (right fst) (runCRandT m g)
{-# INLINE evalCRandT #-}

runCRand :: (ContainsGenError e) => CRand g e a -> g -> Either e (a, g)
runCRand m = runIdentity . runCRandT m
{-# INLINE runCRand #-}

evalCRand :: CRand g GenError a -> g -> Either GenError a
evalCRand m = runIdentity . evalCRandT m
{-# INLINE evalCRand #-}

instance (ContainsGenError e, Monad m, CryptoRandomGen g) => MonadCRandom e (CRandT g e m) where
  getCRandom  = wrap crandom
  {-# INLINE getCRandom #-}
  getBytes i = wrap (genBytes i)
  {-# INLINE getBytes #-}
  getBytesWithEntropy i e = wrap (genBytesWithEntropy i e)
  {-# INLINE getBytesWithEntropy #-}
  doReseed bs = CRandT $ do
                        get >>= \g ->
                         case reseed bs g of
                            Right g' -> put g'
                            Left  x  -> throwError (fromGenError x)
  {-# INLINE doReseed #-}

instance (ContainsGenError e, Monad m, CryptoRandomGen g) => MonadCRandomR e (CRandT g e m) where
  getCRandomR = wrap . crandomR
  {-# INLINE getCRandomR #-}

instance MonadCRandomR e m => MonadCRandomR e (Lazy.StateT s m) where
  getCRandomR = lift . getCRandomR
  {-# INLINE getCRandomR #-}

instance MonadCRandomR e m => MonadCRandomR e (Strict.StateT s m) where
  getCRandomR = lift . getCRandomR
  {-# INLINE getCRandomR #-}

instance (MonadCRandomR e m, Monoid w) => MonadCRandomR e (Lazy.WriterT w m) where
  getCRandomR = lift . getCRandomR
  {-# INLINE getCRandomR #-}

instance (MonadCRandomR e m, Monoid w) => MonadCRandomR e (Strict.WriterT w m) where
  getCRandomR = lift . getCRandomR
  {-# INLINE getCRandomR #-}

instance MonadCRandomR e m => MonadCRandomR e (ReaderT r m) where
  getCRandomR = lift . getCRandomR
  {-# INLINE getCRandomR #-}

instance (MonadCRandomR e m, Monoid w) => MonadCRandomR e (Lazy.RWST r w s m) where
  getCRandomR = lift . getCRandomR
  {-# INLINE getCRandomR #-}

instance (MonadCRandomR e m, Monoid w) => MonadCRandomR e (Strict.RWST r w s m) where
  getCRandomR = lift . getCRandomR
  {-# INLINE getCRandomR #-}

base2Log :: Integer -> Integer
base2Log i
  | i >= setBit 0 64 = 64 + base2Log (i `shiftR` 64)
  | i >= setBit 0 32 = 32 + base2Log (i `shiftR` 32)
  | i >= setBit 0 16 = 16 + base2Log (i `shiftR` 16)
  | i >= setBit 0 8  = 8  + base2Log (i `shiftR` 8)
  | i >= setBit 0 0  = 1  + base2Log (i `shiftR` 1)
  | otherwise        = 0

bs2i :: B.ByteString -> Integer
bs2i bs = B.foldl' (\i b -> (i `shiftL` 8) + fromIntegral b) 0 bs
{-# INLINE bs2i #-}
