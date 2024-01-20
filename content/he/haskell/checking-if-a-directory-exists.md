---
title:                "בדיקה אם ספרייה קיימת"
html_title:           "Haskell: בדיקה אם ספרייה קיימת"
simple_title:         "בדיקה אם ספרייה קיימת"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?
בדיקה אם ספריה קיימת היא מנגנון של שפה התכנות מאפשר למתכנת לבדוק אם הספריה שהוזנה כארגומנט נמצאת במערכת. התכנתים עושים את זה כדי למנוע שגיאות מיותרות בזמן הריצה.

## איך לבצע:
```
import System.Directory

main :: IO ()
main = do
    putStrLn "Please, enter the directory you want to check:"
    dirName <- getLine
    doesDirExist <- doesDirectoryExist dirName
    putStrLn $ "Does the directory exist? " ++ show doesDirExist
```
כאשר מריצים את דוגמא זו, ראשית הוא יבקש ממך שם הספריה, ואז הוא יבדוק אם הספריה התואמת שם זה ממש קיימת במערכת.

## שיעורים מעומקים:
1. בהקשר היסטורי, הפונקציה "doesDirectoryExist" התחזקה בהקלה משום ש2GNU Haskell ISA מאוד משמש.
2. אלטרנטיבות כוללות השתמשות בספריות חיצוניות שנותנות יותר שליטה, אך ייתכן שיהיו איכשהו פחות תודעתיים.
3. מאחורי הקלעים, "doesDirectoryExist" מבצעת בדיקה מחודשת לתת-ספריה כאשר התכנית האשלית מתורגמת.

## ראה גם:
1. [תיעוד של System.Directory](https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html)