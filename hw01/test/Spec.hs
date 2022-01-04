import Lib (toDigitsRev, toDigits, doubleEveryOther, sumDigits, validate, hanoi)
import Control.Monad (when)

test_list_equal :: [Integer] -> [Integer] -> Bool
test_list_equal [] [] = True
test_list_equal [] (x:xs) = False
test_list_equal (x:xs) [] = False
test_list_equal list1 list2
    | head list1 == head list2 = test_list_equal (tail list1) (tail list2)
    | otherwise = False


test_toDigit :: Integer -> [Integer] -> Bool
test_toDigit num = test_list_equal (toDigits num)

main :: IO ()
main = do
    putStrLn ""
    putStrLn $ if test_toDigit 1234 [1,2,3,4] then "OK" else "Fail"
    putStrLn $ if test_toDigit 0 [] then "OK" else "Fail"
    putStrLn $ if test_toDigit (-17) [] then "OK" else "Fail"
    putStrLn $ if doubleEveryOther [1,2,3,4] == [2,2,6,4] then "OK" else "Fail"
    putStrLn $ if sumDigits [16,7,12,5] == 22 then "OK" else "Fail"
    putStrLn $ if validate 4012888888881881  then "OK" else "Fail"
    putStrLn $ if not $ validate 4012888888881882 then "OK" else "Fail"
    print $ hanoi 3 "a" "c" "b"
    return ()
