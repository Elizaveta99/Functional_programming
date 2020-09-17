sinusX::Double -> Double
sinusXHelper::Double -> Double -> Double ->Double -> Double -> Double

sinusXHelper cur x n s eps 
    | (abs cur) <= eps = s
    | otherwise = sinusXHelper (((-cur)*x*x)/(n+1)/(n+2)) x (n+2) (s+cur) eps

sinusX x = sinusXHelper x x 1 0 0.000000001 