{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Vec where

import Data.Functor (($>))
import Data.Kind
import GHC.Ix
import GHC.TypeLits
import Text.ParserCombinators.ReadP
import Text.Read

data Vec :: Natural -> Type -> Type where
  Nil :: Vec 0 a
  Cons :: a -> Vec n a -> Vec (succ n) a

deriving instance (Eq a) => Eq (Vec n a)

deriving instance (Ord a) => Ord (Vec n a)

deriving instance Functor (Vec n)

instance (Ix a) => Ix (Vec 0 a) where
  range (n, _) = [n]
  index _ _ = 0
  inRange _ _ = True

instance (Ix a, Ix (Vec n a)) => Ix (Vec (succ n) a) where
  range (Cons n ns, Cons m ms) = [Cons i is | i <- range (n, m), is <- range (ns, ms)]
  unsafeIndex (Cons n ns, Cons m ms) (Cons i is) = unsafeIndex (n, m) i + unsafeRangeSize (n, m) * unsafeIndex (ns, ms) is
  inRange (Cons n ns, Cons m ms) (Cons i is) = inRange (n, m) i && inRange (ns, ms) is

instance (Read a) => Read (Vec 0 a) where
  readPrec = pure Nil

instance (Read a, Read (Vec n a)) => Read (Vec (succ n) a) where
  readPrec = lift (string "[") *> (Cons <$> readPrec <* lift (string "," <* skipSpaces) <*> readPrec) <* lift (string "]")
