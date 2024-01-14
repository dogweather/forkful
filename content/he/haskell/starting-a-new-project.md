---
title:                "Haskell: התחלת פרויקט חדש"
programming_language: "Haskell"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## למה

כשאת/ה מתחיל/ה פרויקט חדש ב-Haskell, את/ה מכיר/ה שאת/ה בדרך לכתוב קוד נקי ויעיל בפונקציונליות מתקדמת ותמיכה נרחבת בסטנדרט הכתיבה. כמו כן, יתרומת השקצות תוכנה מגוונת ומתםלים לשפה הופכת אותה לכלי עבודה חזק ונמתח למתכנתים.

## איך לעשות את זה

```Haskell
main = putStrLn "שלום עולם!" 
```

כאן אנו משתמשים בפונקציה `putStrLn` כדי להדפיס את המחרוזת "שלום עולם!" לטרמינל.

```Haskell
-- פונקציה רקורסיבית לחישוב חזקה
powerOf :: Int -> Int -> Int
powerOf x 0 = 1
powerOf x n = x * powerOf x (n-1)

-- קריאה לפונקציה עם פרמטרים שנכנסים מתוך טקסט כניסה
main = do
  input <- getLine
  let xs = map read $ words input :: [Int] -- יצירת רשימת מספרים מהקלט
  let base = xs !! 0 -- המספר הראשון ברשימה יהיה הבסיס של החזקה
  let exponent = xs !! 1 -- המספר השני ברשימה יהיה החזקה
  let result = powerOf base exponent
  print result -- הדפסת התוצאה לטרמינל
```

כאן אנו משתמשים בפונקציות כמו `getLine` ו-`print` כדי לקבל קלט מהמשתמש ולהדפיס תוצאה לפי הקלט הזה.

## כיכוב מעומק

כאשר את/ה מתחיל/ה פרויקט חדש ב-Haskell, ישנם כמה דברים שיש לקחת בחשבון. כדי לנצל את היתרונות של השפה, חשוב להתרגל לכתיבה בפונקציות ולא להתעניין באורטוגונליות מיותרת. כמו כן, חשוב להשתמש בספריות נתמכות כדי לאפשר פתרונות חכמים יותר ולאחסן קוד נכון יותר.

## ר