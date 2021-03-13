convertRad :: Floating a => a -> a
convertRad a = a*(180/pi)

convertGrau :: Floating a => a -> a
convertGrau a = a*(pi/180)

complexaparapolar :: Floating a => a -> a -> [a]
complexaparapolar a b = [sqrt(a**2 + b**2), convertRad(atan(b/a))]

polarparacomplexa :: (Ord a, Floating a) => a -> a -> [a]
polarparacomplexa a b = if b < 0 then [a*cos(convertGrau(360+b)), a*sin(convertGrau(360+b))] else [a*cos (convertGrau b), a*sin (convertGrau b)]

main :: IO()
main = do
    print("polarparacomplexa")
    print("complexaparapolar")

    