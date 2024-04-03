---
date: 2024-01-20 17:35:39.751116-07:00
description: "\u05D4\u05E6\u05DE\u05D3\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA\
  \ \u05D4\u05D9\u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05D1\u05D4 \u05D0\u05E0\
  \u05D5 \u05DE\u05D7\u05D1\u05E8\u05D9\u05DD \u05E9\u05EA\u05D9 \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05D5\u05EA \u05D0\u05D5 \u05D9\u05D5\u05EA\u05E8 \u05DC\u05D9\u05E6\
  \u05D9\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D0\u05D7\u05EA \u05D2\
  \u05D3\u05D5\u05DC\u05D4 \u05D9\u05D5\u05EA\u05E8. \u05EA\u05DB\u05E0\u05D9\u05EA\
  \u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\
  \u05D9 \u05DC\u05D1\u05E0\u05D5\u05EA \u05D8\u05E7\u05E1\u05D8\u05D9\u05DD \u05D3\
  \u05D9\u05E0\u05DE\u05D9\u05D9\u05DD, \u05DC\u05D4\u05D5\u05E1\u05D9\u05E3 \u05EA\
  \u05DB\u05E0\u05D9\u05DD, \u05D0\u05D5\u2026"
lastmod: '2024-03-13T22:44:39.399828-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05E6\u05DE\u05D3\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA\
  \ \u05D4\u05D9\u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05D1\u05D4 \u05D0\u05E0\
  \u05D5 \u05DE\u05D7\u05D1\u05E8\u05D9\u05DD \u05E9\u05EA\u05D9 \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05D5\u05EA \u05D0\u05D5 \u05D9\u05D5\u05EA\u05E8 \u05DC\u05D9\u05E6\
  \u05D9\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D0\u05D7\u05EA \u05D2\
  \u05D3\u05D5\u05DC\u05D4 \u05D9\u05D5\u05EA\u05E8."
title: "\u05E9\u05E8\u05E9\u05D5\u05E8 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA"
weight: 3
---

## איך לעשות:
בהסקל, הפעולה הכי בסיסית להצמדת מחרוזות היא על ידי שימוש באופרטור `++`.

```Haskell
main :: IO ()
main = do
    let hello = "שלום"
    let world = "עולם"
    let greeting = hello ++ ", " ++ world ++ "!"
    putStrLn greeting
```

פלט:
```
שלום, עולם!
```

או על ידי שימוש בפונקציית `concat` כדי לחבר רשימת מחרוזות:

```Haskell
main :: IO ()
main = do
    let wordsList = ["שלום", " ", "עולם", "!"]
    let greeting = concat wordsList
    putStrLn greeting
```

פלט:
```
שלום עולם!
```

## צלילה לעומק
הצמדת מחרוזות היא פעולה נפוצה במרבית שפות התכנות, והיא הייתה קיימת כבר מאז ימי הקוד הראשונים. בהסקל, שפה פונקציונלית, הפעולה מנוצלת לעיבוד טקסטים וביצוע גריסות עם תבניות מידע מורכבות.

אלטרנטיבות להצמדת מחרוזות כוללות שימוש בפונקציות כמו `intercalate` מהמודול `Data.List` שמאפשרת הצמדת מחרוזות עם תו מפריד:

```Haskell
import Data.List (intercalate)

main :: IO ()
main = do
    let greeting = intercalate ", " ["שלום", "עולם"]
    putStrLn (greeting ++ "!")
```

פלט:
```
שלום, עולם!
```

בעת הצמדת מחרוזות, חשוב לזכור שבהסקל, מחרוזות הן רשימות של תווים. פעולות על רשימות יכולות להיות לא יעילות במיוחד אם מדובר ברשימות ארוכות, מה שעלול להשפיע על הביצועים כאשר מבצעים הצמדות בלולאה או בפונקציות רקורסיביות.

## גם זה רלוונטי:
- [Hackage: Data.List Module](https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-List.html)
- [LYAH: Learn You a Haskell for Great Good! - Starting Out](http://learnyouahaskell.com/starting-out#hello-world)
