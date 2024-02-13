module Eq3 (Eq3((===))) where
import Bool3

class Eq3 a where
  (===) :: a -> a -> Bool3

instance Eq3 Bool3 where
  _ === Unk3 = Unk3
  Unk3 === _ = Unk3
  False3 === True3 = False3
  True3 === True3 = True3
  False3 === False3 = True3
  _ === _ = False3

instance (Eq3 m) => Eq3 (Maybe m) where
  Just x === Just y = (x === y)
  _ === Nothing = Unk3
  Nothing === _ = Unk3
