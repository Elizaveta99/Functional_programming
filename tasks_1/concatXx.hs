conсatXx::[[a]] -> [a]
conсatXx [] = []
conсatXx ([]:ys) = conсatXx ys
conсatXx ((x:xs):ys) = x:conсatXx(xs:ys)