{-# LANGUAGE DataKinds, GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

import GHC.TypeLits
import Data.Proxy (Proxy (Proxy))
import Unsafe.Coerce (unsafeCoerce)

data Nat2 where
  NZ :: Nat2
  NS :: Nat2 -> Nat2

type family Add2' (x :: Nat2) (y :: Nat2) :: Nat2 where
  Add2' NZ y = y
  Add2' (NS x) y = NS (Add2' x y)

type family Mul2' (x :: Nat2) (y :: Nat2) :: Nat2 where
  Mul2' NZ y = NZ
  Mul2' (NS x) y = Add2' y (Mul2' x y)

type family Nat2ToNat (n :: Nat2) :: Nat where
  Nat2ToNat NZ = 0
  Nat2ToNat (NS n) = 1 + Nat2ToNat n
type family NTN2 (n :: Nat) :: Nat2 where
  NTN2 0 = NZ
  NTN2 n = NS (NTN2 (n - 1))

data List a where
  Nil :: List a
  (:::) :: a -> List a -> List a
infixr 5 :::

-- List natural holder
data Holder (x :: List Nat)
data HolderNat (x :: Nat)

instance Show (Holder Nil) where
  show _ = "Nil"
instance (KnownNat n, Show (Holder xs)) => Show (Holder (n ::: xs)) where
  show _ = show (natVal (Proxy :: Proxy n)) ++ " ::: " ++ show (undefined :: Holder xs)

instance (KnownNat n) =>  Show (HolderNat n) where
  show _ = show (natVal (Proxy :: Proxy n))

type (>) (x :: Nat) (y :: Nat) = y + 1 <= x
class Mul (x :: Nat) (y :: Nat) (z :: Nat) | x y -> z
instance Mul x 0 0
instance (y1 > 0, y0 ~ y1 - 1, Mul x y0 p0, p1 ~ p0 + x) => Mul x y1 p1


class Append (a :: List k) (b :: List k) (c :: List k) | a b -> c
instance Append Nil ys ys
instance Append xs ys zs => Append (x ::: xs) ys (x ::: zs)

test :: Append (1 ::: 2 ::: Nil) (3 ::: 4 ::: Nil) z => Holder (z :: List Nat)
test = undefined -- This works (thanks to functional dependencies)

test2 :: Append x (3 ::: 4 ::: Nil) (1 ::: 2 ::: 3 ::: 4 ::: Nil) => Holder (x :: List Nat)
test2 = undefined -- This doesn't work (functional dependencies fault)

class Factorial (n :: Nat2) (f :: Nat2) | n -> f
instance (x ~ NZ) => Factorial NZ (NS NZ)
instance (Factorial x t, Mul2' (NS x) t ~ f) => Factorial (NS x) f

test3 :: Factorial (NTN2 5) r => HolderNat ((Nat2ToNat r) :: Nat)
test3 = undefined -- This works (with some op tricks tho). Btw, nice
