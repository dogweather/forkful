---
title:                "Haskell: כתיבה אל תוך השגיאה התקנית"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## למה

כתיבה למטרת שכיתה שונה בכינונים ושהיא בטוחה יותר למחברת עם פלט תקין ונתונים.

## איך לעשות זאת

```Haskell
main :: IO ()
main = do
  let x = 5
      y = 10
  putStrLn $ "כינון " ++ show x ++ " הוא " ++ show (x * y)
```

פלט:

```
כינון 5 הוא 50
```

## חקירה מעמיקה

כאשר נכנסים לעולם של Haskell, נתקלים במונחים כמו "IO" ו "putStrLn" שבעצם מבצעים את פעולת הכתיבה ל-standard error. זה מאפשר לנו להפעיל את הפונקציות בתוך ה-IO context ולתפוס פלט שלהם במקום הנכון. כדי לפנות לנתונים לכתיבה ל-standard error, ניתן להשתמש בפונקציות כמו "hPutStrLn" או "hPutStr".

## ראה גם

- [המדריך המבואי ל-Haskell](https://wiki.haskell.org/Introduction)
- [הספר "Real World Haskell"](http://book.realworldhaskell.org/read/)
- [מדריך לשפת Haskell בעברית](https://yalh.io/he/)