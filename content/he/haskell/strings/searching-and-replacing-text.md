---
aliases:
- /he/haskell/searching-and-replacing-text/
date: 2024-01-20 17:58:41.301545-07:00
description: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA\
  \ \u05D8\u05E7\u05E1\u05D8 \u05D4\u05D5\u05D0 \u05D4\u05EA\u05D4\u05DC\u05D9\u05DA\
  \ \u05E9\u05D1\u05D5 \u05D0\u05E0\u05D7\u05E0\u05D5 \u05DE\u05D0\u05EA\u05E8\u05D9\
  \u05DD \u05E8\u05E6\u05E4\u05D9\u05DD \u05E9\u05DC \u05EA\u05D5\u05D5\u05D9\u05DD\
  \ \u05D1\u05D8\u05E7\u05E1\u05D8 \u05D5\u05DE\u05D7\u05DC\u05D9\u05E4\u05D9\u05DD\
  \ \u05D0\u05D5\u05EA\u05DD \u05D1\u05E8\u05E6\u05E4\u05D9\u05DD \u05D0\u05D7\u05E8\
  \u05D9\u05DD. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05EA\u05E7\u05DF \u05E9\
  \u05D2\u05D9\u05D0\u05D5\u05EA, \u05DC\u05E2\u05D3\u05DB\u05DF \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD, \u05D0\u05D5 \u05DC\u05D1\u05E6\u05E2\u2026"
lastmod: 2024-02-18 23:08:52.871561
model: gpt-4-1106-preview
summary: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA \u05D8\
  \u05E7\u05E1\u05D8 \u05D4\u05D5\u05D0 \u05D4\u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\
  \u05D1\u05D5 \u05D0\u05E0\u05D7\u05E0\u05D5 \u05DE\u05D0\u05EA\u05E8\u05D9\u05DD\
  \ \u05E8\u05E6\u05E4\u05D9\u05DD \u05E9\u05DC \u05EA\u05D5\u05D5\u05D9\u05DD \u05D1\
  \u05D8\u05E7\u05E1\u05D8 \u05D5\u05DE\u05D7\u05DC\u05D9\u05E4\u05D9\u05DD \u05D0\
  \u05D5\u05EA\u05DD \u05D1\u05E8\u05E6\u05E4\u05D9\u05DD \u05D0\u05D7\u05E8\u05D9\
  \u05DD. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\
  \u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05EA\u05E7\u05DF \u05E9\u05D2\
  \u05D9\u05D0\u05D5\u05EA, \u05DC\u05E2\u05D3\u05DB\u05DF \u05E0\u05EA\u05D5\u05E0\
  \u05D9\u05DD, \u05D0\u05D5 \u05DC\u05D1\u05E6\u05E2\u2026"
title: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA \u05D8\
  \u05E7\u05E1\u05D8"
---

{{< edit_this_page >}}

## מה ולמה?
חיפוש והחלפת טקסט הוא התהליך שבו אנחנו מאתרים רצפים של תווים בטקסט ומחליפים אותם ברצפים אחרים. תכניתנים עושים זאת כדי לתקן שגיאות, לעדכן נתונים, או לבצע פורמטינג מחדש בקוד ובמסמכים.

## איך לעשות:
ב-Haskell, אפשר לממש חיפוש והחלפה בקלות עם מודול `Data.Text`:

```Haskell
import Data.Text (Text, replace, pack, unpack)

searchAndReplace :: String -> String -> String -> String
searchAndReplace old new = unpack . replace (pack old) (pack new) . pack

main :: IO ()
main = putStrLn $ searchAndReplace "world" "Haskell" "Hello, world!"
```

פלט:
```
Hello, Haskell!
```

## ניחוח לעומק
החלפת טקסט היא פעולה עתיקה ככתיבה עצמה, אבל בתכנות היא נעשית חשובה במיוחד עם העלייה בשימוש בעיבוד טקסט אוטומטי. בהקשר של Haskell, קיימים כמה מודולים חלופיים לעיבוד טקסט, כולל `Text.Regex` שמתמקד בביטויים רגולריים, אולם `Data.Text` הוא פשוט ויעיל לרוב המקרים. יתרון של `Data.Text` הוא שהוא מאוחסן בזיכרון בצורה יעילה יותר ממחרוזות רגילות (`String`), ולכן גם פעולות כמו חיפוש והחלפה רצות מהר יותר.

## ראו גם
- התיעוד של `Data.Text` ב-Hackage: [Data.Text](https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html)
- מדריך לביטויים רגולריים ב-Haskell: [Text.Regex](https://hackage.haskell.org/package/regex-base-0.94.0.1/docs/Text-Regex-Base.html)
- ספר על עיבוד טקסט וביטויים רגולריים ב-Haskell: "Real World Haskell" פרק 8: [Real World Haskell](http://book.realworldhaskell.org/read/efficient-file-processing-regular-expressions-and-file-name-matching.html)
