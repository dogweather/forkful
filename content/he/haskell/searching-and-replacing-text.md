---
title:                "חיפוש והחלפת טקסט"
date:                  2024-01-20T17:58:41.301545-07:00
model:                 gpt-4-1106-preview
simple_title:         "חיפוש והחלפת טקסט"

category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/searching-and-replacing-text.md"
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
