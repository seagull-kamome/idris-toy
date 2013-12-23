module Logic.Classic

import Logic.Basic

%access public
%default total

to_dne : a -> Not (Not a)
to_dne x y = y x

elim_dne : Not (Not a) -> a -- ((a -> _|_) -> _|_) -> a
elim_dne = ?elim_dne_1

em : Not a \/ a
em = ?em_1

peirce : ((p -> q) -> p) -> p   -- C.P.S!C.P.S!
peirce f = believe_me (\k => k (f k))




