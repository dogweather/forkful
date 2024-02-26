---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:45.918227-07:00
description: "\u05D4\u05E4\u05D9\u05DB\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  \ \u05DC\u05D0\u05D5\u05EA \u05E8\u05D0\u05E9\u05D9\u05EA \u05D2\u05D3\u05D5\u05DC\
  \u05D4 \u05DB\u05D5\u05DC\u05DC\u05EA \u05D4\u05DE\u05E8\u05EA \u05D4\u05D0\u05D5\
  \u05EA \u05D4\u05E8\u05D0\u05E9\u05D5\u05E0\u05D4 \u05E9\u05DC \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA \u05E0\u05EA\u05D5\u05E0\u05D4 \u05DC\u05D0\u05D5\u05EA \u05D2\
  \u05D3\u05D5\u05DC\u05D4 \u05EA\u05D5\u05DA \u05DB\u05D3\u05D9 \u05D4\u05D1\u05D8\
  \u05D7\u05D4 \u05E9\u05E9\u05D0\u05E8 \u05D4\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA\
  \ \u05D9\u05E9\u05D0\u05E8\u05D5 \u05E7\u05D8\u05E0\u05D5\u05EA. \u05DE\u05EA\u05DB\
  \u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DC\
  \u05E6\u05D5\u05E8\u05DB\u05D9 \u05E2\u05D9\u05E6\u05D5\u05D1\u2026"
lastmod: '2024-02-25T18:49:37.624282-07:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05E4\u05D9\u05DB\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\
  \u05D0\u05D5\u05EA \u05E8\u05D0\u05E9\u05D9\u05EA \u05D2\u05D3\u05D5\u05DC\u05D4\
  \ \u05DB\u05D5\u05DC\u05DC\u05EA \u05D4\u05DE\u05E8\u05EA \u05D4\u05D0\u05D5\u05EA\
  \ \u05D4\u05E8\u05D0\u05E9\u05D5\u05E0\u05D4 \u05E9\u05DC \u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05EA \u05E0\u05EA\u05D5\u05E0\u05D4 \u05DC\u05D0\u05D5\u05EA \u05D2\u05D3\
  \u05D5\u05DC\u05D4 \u05EA\u05D5\u05DA \u05DB\u05D3\u05D9 \u05D4\u05D1\u05D8\u05D7\
  \u05D4 \u05E9\u05E9\u05D0\u05E8 \u05D4\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05D9\
  \u05E9\u05D0\u05E8\u05D5 \u05E7\u05D8\u05E0\u05D5\u05EA. \u05DE\u05EA\u05DB\u05E0\
  \u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DC\u05E6\
  \u05D5\u05E8\u05DB\u05D9 \u05E2\u05D9\u05E6\u05D5\u05D1\u2026"
title: "\u05D4\u05D2\u05D3\u05DC\u05EA \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05D1\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
הפיכת מחרוזת לאות ראשית גדולה כוללת המרת האות הראשונה של מחרוזת נתונה לאות גדולה תוך כדי הבטחה ששאר האותיות ישארו קטנות. מתכנתים עושים זאת לצורכי עיצוב פלטים, הקפדה על נכונות דקדוקית בטקסטים, או שיפור קריאות הנתונים שנוצרים.

## איך לעשות:
ב-Haskell, ניתן להפוך מחרוזת לאות ראשית גדולה באמצעות הספרייה הסטנדרטית ללא צורך בספריות צד שלישי.

```haskell
import Data.Char (toUpper, toLower)

capitalize :: String -> String
capitalize "" = ""
capitalize (head:tail) = toUpper head : map toLower tail

-- שימוש לדוגמה:
main = putStrLn $ capitalize "hello world"
```

פלט:
```
Hello world
```

לסיטואציות מורכבות יותר או לנוחות שימוש, ייתכן שתרצו להשתמש בספרייה צד שלישי כמו `text`, שהיא פופולרית לניהול מחרוזות ביעילות ב-Haskell.

ראשית, עליכם להוסיף את `text` לתלות בפרויקט שלכם. לאחר מכן, תוכלו להשתמש בפונקציות שלה להפוך מחרוזת לאות ראשית גדולה כך:

```haskell
import qualified Data.Text as T
import Data.Char (toUpper)

capitalizeText :: T.Text -> T.Text
capitalizeText text = case T.uncons text of
    Nothing -> T.empty
    Just (first, rest) -> T.cons (toUpper first) (T.toLower rest)

-- שימוש לדוגמה עם הספרייה text:
main = putStrLn $ T.unpack $ capitalizeText (T.pack "hello world")
```

פלט:
```
Hello world
```

שני הדוגמאות הללו מדגימות דרכים פשוטות אך יעילות להפוך מחרוזת לאות ראשית גדולה ב-Haskell, עם או ללא ספריות צד שלישי.
