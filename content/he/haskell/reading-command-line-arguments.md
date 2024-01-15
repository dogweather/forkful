---
title:                "קריאת ארגומנטים בשורת פקודה"
html_title:           "Haskell: קריאת ארגומנטים בשורת פקודה"
simple_title:         "קריאת ארגומנטים בשורת פקודה"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## מדוע

למה אנשים יירצו לקרוא על קלטי שורת פקודה? פקודות שורת הפקודה הן דרך נוחה להפעיל תוכניות ולהעביר להן פרמטרים בזמן הפעלה. במאמר זה נלמד כיצד לטפל בקלטי פקודה בשפת התכנות Haskell כדי למקסם את התועלת שאנו יכולים לקבל מתוכניות שורת הפקודה.

## כיצד לעשות זאת

את הפונקציות הדרושות לקריאת קלטי שורת הפקודה כבר יש בסטנדרט המובנה של Haskell. תחילה, ניצור קובץ קוד חדש בשם "command-line-arguments.hs". נשתמש בדפדפן האינטרנט שלנו לכתיבת הקוד הבא:

```haskell
-- ייבוא טיפוסים דרושים מתוך ספריית System.Environment
import System.Environment

-- פונקציה שמדפיסה את כל הפרמטרים שהועברו בקלט שורת הפקודה
main = do
    -- קבלת קלטי שורת פקודה ושמירתם ברשימה
    args <- getArgs
    -- פקודה להדפסת הרשימה של קלטי הפקודה
    print args
```

עם כניסה כלשהי לשורת הפקודה תמתין לולאת השורת שורתך לבצע את הפקודה. נפעיל את הקובץ בשם "command-line-arguments.hs" בעזרת מה שנרשמכ בתוך שורת האינטרנט הבאה:

```bash
ghc -o command-line-arguments command-line-arguments.hs 
```

זה ייצור את הקובץ המכיל את הקובץ הנבחר (command-line-arguments) כאשר נקרא אליו. לאחר שזה נעשה בהצלחה, יש לאחכ אל וסת "command-line-arguments".

```bash
./command-line-arguments hello world
```

להודעה שמופיעה יש לשם קבצי אחרות שנעברו בנעניםיה הפקודות.

```bash
["hello", "world"]
```

פונקציות אלה, getArgs וכן getProgName, משמשות כדיאוז לזהא הם נתכץל