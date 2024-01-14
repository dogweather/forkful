---
title:                "Haskell: מאמתים אם תיקייה קיימת"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## למה?

ביקורת קוד מורכבת עוזרת להבטיח שהתיקייה שאנו מנסים לטעון קיימת במערכת ההפעלה שלנו. זה מאפשר לנו לבנות קוד בטוח ומהיר יותר, ביותר קלות כאשר נמצא בשלב תכנות ומסיימים כרגע.

## כיצד לבדוק אם תיקייה קיימת בקוד Haskell

אם ברצונכם לבדוק האם תיקייה מסוימת קיימת במערכת ההפעלה, תוכלו להשתמש בפונקציה `doesDirectoryExist` בספריה של `System.Directory`.

```
Haskell
import System.Directory (doesDirectoryExist)

main :: IO ()
main = do
    dirExists <- doesDirectoryExist "path/to/directory"
    if dirExists
        then putStrLn "The directory exists."
        else putStrLn "The directory does not exist."
```

תוצאה:

```
The directory exists.
```

אם התיקייה לא קיימת במערכת ההפעלה, תקבלו את ההודעה הבאה:

```
The directory does not exist.
```

## העמקה נוספת

ביקורת קוד עם `System.Directory` שימושית גם לזיהוי אם תיקייה מסוימת היא באמת תיקייה או קובץ רגיל. זה יכול להיות שימושי במצבים כמו קריאת קבצים או ביצוע פעולות במשתנים שמייצגים מסלולים לתיקיות.

## ראו גם

- [דוקומנטציית `System.Directory`](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [ביקורת קוד ב-Haskell עבור מתחילים](https://www.haskell.org/tutorial/basics.html)