---
date: 2024-01-20 17:42:59.890763-07:00
description: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05E9\
  \u05EA\u05D5\u05D0\u05DE\u05D9\u05DD \u05D3\u05E4\u05D5\u05E1 \u05D4\u05D9\u05D0\
  \ \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05D1\u05D4 \u05E0\u05E9\u05DC\u05E4\u05D9\
  \u05DD \u05EA\u05D5\u05D5\u05D9\u05DD \u05DE\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA \u05D0\u05DD \u05D4\u05DD \u05E2\u05D5\u05E0\u05D9\u05DD \u05E2\
  \u05DC \u05EA\u05E0\u05D0\u05D9 \u05DE\u05E1\u05D5\u05D9\u05DD. \u05DE\u05EA\u05DB\
  \u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D6\
  \u05D4 \u05DC\u05E0\u05D9\u05E7\u05D5\u05D9 \u05E7\u05DC\u05D8, \u05EA\u05D9\u05E7\
  \u05D5\u05DF \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D5\u05E2\u05D9\u05D1\u05D5\
  \u05D3 \u05D8\u05E7\u05E1\u05D8."
lastmod: '2024-03-11T00:14:12.839759-06:00'
model: gpt-4-1106-preview
summary: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05E9\u05EA\
  \u05D5\u05D0\u05DE\u05D9\u05DD \u05D3\u05E4\u05D5\u05E1 \u05D4\u05D9\u05D0 \u05E4\
  \u05E2\u05D5\u05DC\u05D4 \u05E9\u05D1\u05D4 \u05E0\u05E9\u05DC\u05E4\u05D9\u05DD\
  \ \u05EA\u05D5\u05D5\u05D9\u05DD \u05DE\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05EA \u05D0\u05DD \u05D4\u05DD \u05E2\u05D5\u05E0\u05D9\u05DD \u05E2\u05DC\
  \ \u05EA\u05E0\u05D0\u05D9 \u05DE\u05E1\u05D5\u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\
  \u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D6\u05D4\
  \ \u05DC\u05E0\u05D9\u05E7\u05D5\u05D9 \u05E7\u05DC\u05D8, \u05EA\u05D9\u05E7\u05D5\
  \u05DF \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D5\u05E2\u05D9\u05D1\u05D5\u05D3\
  \ \u05D8\u05E7\u05E1\u05D8."
title: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05D4\u05EA\
  \u05D5\u05D0\u05DE\u05D9\u05DD \u05DC\u05EA\u05D1\u05E0\u05D9\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
מחיקת תווים שתואמים דפוס היא פעולה שבה נשלפים תווים מתוך מחרוזת אם הם עונים על תנאי מסוים. מתכנתים משתמשים בזה לניקוי קלט, תיקון נתונים ועיבוד טקסט.

## איך לעשות:
ב-Haskell, ניתן להשתמש בפונקציות סטנדרטיות כמו `filter` למימוש התהליך:

```Haskell
import Data.Char (isDigit)

-- פונקציה למחיקת כל הספרות מתוך מחרוזת
deleteDigits :: String -> String
deleteDigits = filter (not . isDigit)

main :: IO ()
main = putStrLn $ deleteDigits "He7ll9o W0or6ld"  -- תוצאה: Hello World
```

ניתן גם להגדיר דפוסי regex ולהשתמש בם למחיקת תווים:

```Haskell
import Text.Regex.TDFA ((=~))

-- פונקציה למחיקת כל האותיות הגדולות מתוך מחרוזת
deleteUpperCase :: String -> String
deleteUpperCase str = str =~ "[^A-Z]" :: String

main :: IO ()
main = putStrLn $ deleteUpperCase "Hello World"  -- תוצאה: ello orld
```

## עיון מעמיק
היסטורית, מחיקת תוים תואמי דפוס נוצרה בכדי להתמודד עם טקסטים גולמיים ועלובים. אם פעם נעשה שימוש בכלים כמו `sed` ו`awk` בסביבת יוניקס, היום לשפות תכנות מודרניות כמו Haskell יש ספריות עשירות המאפשרות יישום חלק ויעיל של מניפולציות טקסט.

כלים אלטרנטיביים כוללים שימוש בפונקציות כמו `map`, `concatMap`, או פונקציות ב-slice ו-append למחרוזות. פרטי מימוש כוללים עבודה עם מבני נתונים ברמה נמוכה יותר כדי לטפל ביעילות במחרוזות גדולות.

## ראה גם
- [Hackage: regex-tdfa package](https://hackage.haskell.org/package/regex-tdfa)
- [Learn You a Haskell for Great Good! - פונקציה עבור כל מצב](http://learnyouahaskell.com/higher-order-functions#maps-and-filters)
