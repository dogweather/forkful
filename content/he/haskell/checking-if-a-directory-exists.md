---
title:                "לבדיקה אם קיימת תיקייה"
html_title:           "Haskell: לבדיקה אם קיימת תיקייה"
simple_title:         "לבדיקה אם קיימת תיקייה"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?
בתכנות, בדיקת האם תיקייה קיימת היא פעולה חשובה לשימוש בתיקיות וקבצים במערכת הקבצים של המחשב. זהו דרך לוודא שהתיקייה או הקובץ שאנו רוצים לעבוד עליו באפליקציה שלנו קיים במחשב בזמן הרצת הקוד. מתכנתים משתמשים בבדיקת התיקייה כדי למנוע שגיאות ודפוסי פעולה לתכנית שלהם.

## איך לעשות?
כדי לבדוק אם תיקייה קיימת ב-Haskell, אנחנו משתמשים בפונקציה פנימית שנקראת doesDirectoryExist. ניתן להשתמש בה על מנת לבדוק אם תיקייה קיימת באמצעות הפקודה הבאה:

```Haskell
import System.Directory
 
doesDirectoryExist :: FilePath -> IO Bool
```
פקודה זו תחזיר ערך בוליאני שיציין האם התיקייה קיימת או לא. אם התיקייה קיימת, יחזיר ערך True, אחרת יחזיר ערך False.

## טיפ עמוק
ניתן גם לבדוק אם תיקייה קיימת באמצעות קריאה למערכת הקבצים של המחשב ובדיקת הערך של המשתנה CURRENT_WORKING_DIRECTORY. פתרון הוא פחות אלגנטי ביחס לשימוש בפונקציה doesDirectoryExist, אבל עדיין תקף.

אם במקרה שלכם אין הצורך בבדיקת הקיומו של התיקייה, אלא רק ליצור אותה במקרה שהיא לא קיימת, אפשר להשתמש בפונקציה createDirectory במקום. היא תיצור את התיקייה אם היא אינה קיימת ותחזיר את התיקייה בכל מקרה.

## לקריאה נוספת
* [Haskell 101](https://sebastiandedeyne.com/haskell-101-how-to-check-if-a-directory-exists/)
* [Documentation for System.Directory](https://hackage.haskell.org/package/directory-1.3.6.0/docs/System-Directory.html)