---
title:    "Haskell: עיבוד מחשב - בדיקת קיום תיקייה"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## למה

ברוך הבא לפוסט החדש שלנו למדריך תכנות Haskell עבור קוראיינו הישראלים האוהבים תכנות ועולם הפרוגרמינג! הפוסט היום יעסוק באיך לבדוק האם תיקייה קיימת באמצעות השפה התכנותית Haskell. אנו נסביר למה זה חשוב ואיך ניתן לעשות את זה בפשטות.

## איך לבדוק האם תיקייה קיימת

בכדי לבדוק האם תיקייה קיימת באמצעות Haskell, אנו נשתמש בפונקציית `doesDirectoryExist` מהספרייה `System.Directory`. הנה דוגמא לקוד המבצע את הבדיקה ומחזיר את התוצאה:

```Haskell
import System.Directory

-- בדיקה האם התיקייה "documents" קיימת
doesExist <- doesDirectoryExist "documents"
-- בדיקת התוצאה
if doesExist
  then putStrLn "התיקייה קיימת!"
  else putStrLn "התיקייה לא קיימת."
```

כפי שאתם רואים, הפונקציה מחזירה משתנה בוליאני שמציין האם התיקייה קיימת או לא. אתם יכולים להשתמש בתנאי IF כדי להתאים את התגובה של התוכנית לתוצאה.

## מעמקים

אם תרצו לפתוח תיקייה קיימת או ליצור תיקייה חדשה אם היא לא קיימת, תוכלו להשתמש בפונקציה `createDirectoryIfMissing` מהספרייה `System.Directory`.

```Haskell
import System.Directory

-- פתיחת תיקייה קיימת או יצירת תיקייה חדשה אם היא לא קיימת
createDirectoryIfMissing False "documents"
```

כפי שאתם רואים, ניתן להשתמש בארגומנט בוליאני כדי להגדיר אם ליצור את התיקייה רק אם היא לא קיימת.

## ראו גם

- [Haskell Documentation על הפונקציה `doesDirectoryExist`](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/directory-1.3.6.1/System-Directory.html#v:doesDirectoryExist)
- [Haskell Documentation על הפונקציה `createDirectoryIfMissing`](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/directory-1.3.6.