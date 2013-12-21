module Logic.Classic

import Logic.Basic

%access public
-- %default total

dne : Not (Not a) -> a
dne = believe_me

-- em : Not a \/ a
-- em = believe_me

peirce : ((p -> q) -> p) -> p   -- C.P.S!C.P.S!
peirce f = believe_me $ (\k => k (f k))

