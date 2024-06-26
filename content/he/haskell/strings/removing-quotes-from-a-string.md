---
date: 2024-01-26 03:40:19.549145-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-Haskell, \u05D0\
  \u05E0\u05D5 \u05D9\u05DB\u05D5\u05DC\u05D9\u05DD \u05DC\u05D9\u05E6\u05D5\u05E8\
  \ \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D4 \u05E9\u05DE\u05E1\u05D9\u05E8\u05D4\
  \ \u05D0\u05EA \u05DB\u05DC \u05D4\u05E6\u05D9\u05D8\u05D5\u05D8\u05D9\u05DD \u05DE\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05E0\u05EA\u05D5\u05E0\u05D4. \u05D6\u05D4\
  \ \u05DB\u05DE\u05D5 \u05DC\u05D5\u05DE\u05E8 \u05DC\u05E6\u05D9\u05D8\u05D5\u05D8\
  \u05D9\u05DD \u05DC\u05D6\u05D5\u05D6, \u05D5\u05DC\u05D4\u05D1\u05D8\u05D9\u05D7\
  \ \u05E9\u05D4\u05DD \u05DC\u05D5\u05E7\u05D7\u05D9\u05DD \u05D0\u05EA \u05D4\u05E8\
  \u05DE\u05D6."
lastmod: '2024-03-13T22:44:39.392670-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-Haskell, \u05D0\u05E0\u05D5 \u05D9\u05DB\u05D5\u05DC\u05D9\u05DD\
  \ \u05DC\u05D9\u05E6\u05D5\u05E8 \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D4 \u05E9\
  \u05DE\u05E1\u05D9\u05E8\u05D4 \u05D0\u05EA \u05DB\u05DC \u05D4\u05E6\u05D9\u05D8\
  \u05D5\u05D8\u05D9\u05DD \u05DE\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05E0\u05EA\
  \u05D5\u05E0\u05D4."
title: "\u05D4\u05E1\u05E8\u05EA \u05DE\u05E8\u05DB\u05D0\u05D5\u05EA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 9
---

## איך לעשות:
ב-Haskell, אנו יכולים ליצור פונקציה שמסירה את כל הציטוטים ממחרוזת נתונה. זה כמו לומר לציטוטים לזוז, ולהבטיח שהם לוקחים את הרמז.

```Haskell
import Data.List (intercalate)
import Data.Char (isPunctuation)

removeQuotes :: String -> String
removeQuotes = filter (\c -> c /= '"' && c /= '\'')

main :: IO ()
main = do
    let stringWithQuotes = "Haskell טען, \"בואו נלמד כמה פונקציות!\""
    putStrLn $ removeQuotes stringWithQuotes
```

פלט לדוגמא:

```
Haskell טען, בואו נלמד כמה פונקציות!
```

## צלילה עמוקה
פעם, לפני שמחרוזות בתיכנות היו נפוצות כמו סרטוני חתולים באינטרנט, טיפול בטקסט היה משימה שנויה בקושי. אבל ככל ששפות תכנות התפתחו, מחרוזות הפכו לחלק בלתי נפרד מהתכנות. עם זאת, ציטוטים נשארו חרב פיפיות—חיוניים להגדרת מחרוזות, אך מטרד כשהם כלולים כנתונים אמיתיים.

אלטרנטיבות? במקום לטרוט את כל הציטוטים כמו זבובים, אתה יכול להיות בררני. ייתכן שתרצה להסיר רק את הציטוטים החיצוניים ביותר (גיזום קלאסי) או לטפל בציטוטים מוברחים בתוך מחרוזת.

מבחינה יישומית, הפונקציה `removeQuotes` למעלה משתמשת בלמבדא כדי לבדוק כל תו (`c`) אם הוא ציטוט מטריד ולסנן אותו בהתאם. זהו גישה ישירה, אך עבור טקסטים גדולים יותר או כללים מורכבים יותר, ייתכן שתרצה לבדוק ספריות פרסר כמו `Parsec`, שיכולות לתת לך יותר עדינות וכוח בעיבוד טקסט.

## ראו גם:
- לאוהבי regex: [Text.Regex.Posix](https://hackage.haskell.org/package/regex-posix)
- הקדמה עדינה למחרוזות ב-Haskell: [Learn You a Haskell for Great Good! - התחלה](http://learnyouahaskell.com/starting-out#strings)
