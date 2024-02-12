---
title:                "הסרת מרכאות ממחרוזת"
aliases:
- /he/haskell/removing-quotes-from-a-string.md
date:                  2024-01-26T03:40:19.549145-07:00
model:                 gpt-4-0125-preview
simple_title:         "הסרת מרכאות ממחרוזת"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
הסרת ציטוטים ממחרוזת אומרת לפטר כל סימני הציטוט—יחידים (' ') או כפולים (" ")—שהם חלק מנתוני המחרוזת. מתכנתים עושים זאת כדי לנקות קלטים, להכין טקסט לעיבוד, או להיפטר מתווים מיותרים שעלולים להפריע לטיפול בנתונים ולפעולות.

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
