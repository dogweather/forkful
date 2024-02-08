---
title:                "הגדלת אותיות במחרוזת"
date:                  2024-02-03T19:05:45.918227-07:00
model:                 gpt-4-0125-preview
simple_title:         "הגדלת אותיות במחרוזת"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
