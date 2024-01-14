---
title:                "Haskell: לבדיקה אם ספרייה קיימת"
simple_title:         "לבדיקה אם ספרייה קיימת"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## למה

בעולם של תכנות בHaskell, אחד הדברים החשובים להבנה היא לבדוק אם תיקייה קיימת. במאמר הזה, אני אסביר למה זה חשוב ואיך לבצע זאת בקוד.

## איך לבדוק אם תיקייה קיימת

```Haskell
import System.Directory

main = do
    -- אנו משתמשים בפונקציה doesDirectoryExist
    let path = "path/to/directory"
    dirExists <- doesDirectoryExist path
    -- אם התיקייה קיימת, הערך של dirExists יהיה True, אחרת יהיה False
    if dirExists
        then putStrLn "התיקייה קיימת!"
        else putStrLn "התיקייה לא קיימת."
```

פלט:
``` 
התיקייה קיימת!
```

## מקורות מעמיקים

בנוסף לפונקציה doesDirectoryExist, ישנן עוד פונקציות קשורות לבדיקת תיקיות בחבילת System.Directory של Haskell. נתמקד בכמה מהן:

- `createDirectory` - יוצר תיקייה חדשה בעלת השם שנשלח כפרמטר
- `removeDirectory` - מוחק תיקייה ריקה
- `copyDirectory` - מעתיקת תיקייה ותוכןיה לתוך תיקייה אחרת
- `renameDirectory` - מחליף את שם התיקייה המקורי לשם חדש

כדי להשתמש בפונקציות הללו, נצטרך לייבא אותן מהחבילה System.Directory ולהתאים את הפרמטרים לפי המטרה שלנו.

## ראה גם

בנוסף לחבילת System.Directory, ישנן עוד חבילות בHaskell שמספקות מנגנוני בדיקת תיקיות, כגון:
- `directory` - חבילה שמספקת פונקציות נוספות לניהול תיקיות, תתי תיקיות וקבצים.
- `filepath` - חבילה שמספקת פונקציות לניהול מסלולי קבצים ותיקיות, והמרתה ביניהם.

את החבילות הללו כדאי להתרשם מהתיעוד שנמצא באתר הרשמי של Haskell.