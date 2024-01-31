---
title:                "הדפסת פלט לניפוי באגים"
date:                  2024-01-20T17:52:58.721337-07:00
model:                 gpt-4-1106-preview
simple_title:         "הדפסת פלט לניפוי באגים"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## מה ולמה?
להדפיס פלט דיבאג זה כמו להשתמש במצפן במרחב; זה עוזר לנו לראות את מה שקורה בקוד בזמן אמת. תכנתים עושים את זה לבדוק שהקוד שלהם מתנהג כפי שהם מצפים לו ולאיתור באגים.

## איך לעשות:
```Haskell
main :: IO ()
main = do
    let x = 5
    putStrLn $ "The value of x is: " ++ show x
    -- This will print: The value of x is: 5
```
פלט דוגמה:
```
The value of x is: 5
```
עוד דוגמה:
```Haskell
import Debug.Trace (trace)

main :: IO ()
main = do 
    let list = [1..5]
    let listWithDebug = traceShow list list
    putStrLn $ "Processing list: " ++ show (incrementAll listWithDebug)

incrementAll :: [Int] -> [Int]
incrementAll = map (+1)
-- This will also print the list to the console as a side effect
```
פלט דוגמה:
```
[1,2,3,4,5]
Processing list: [2,3,4,5,6]
```

## עיון נוסף 
בעבר, הדפסה לשם דיבאג הייתה הדרך העיקרית לאיתור באגים. כעת, יש כלים מתקדמים יותר כמו מנתחים ומעקבי קריאה, אבל הדפסות עדיין נפוצות בגלל פשטותן ונוחיותן. ב-Haskell, תכניתנים יכולים להשתמש בפונקציית `print`, הפונקציות שבמודול `Debug.Trace`, או עם כלי דיבאג חיצוניים. זה בחירה של מטרה ועדיפות אישית.

## ראו גם:
- [Haskell Documentation on `Debug.Trace`](https://hackage.haskell.org/package/base-4.16.1.0/docs/Debug-Trace.html)
- [Learn You a Haskell for Great Good - Debugging](http://learnyouahaskell.com/input-and-output#debugging)
- [Real World Haskell - Debugging and Profiling](http://book.realworldhaskell.org/read/profiling-and-optimization.html)
