---
date: 2024-01-20 17:52:58.721337-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05E4\u05DC\u05D8\
  \ \u05D3\u05D5\u05D2\u05DE\u05D4."
lastmod: '2024-04-05T21:53:40.585565-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u05D4\u05D3\u05E4\u05E1\u05EA \u05E4\u05DC\u05D8 \u05DC\u05E0\u05D9\u05E4\
  \u05D5\u05D9 \u05D1\u05D0\u05D2\u05D9\u05DD"
weight: 33
---

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
