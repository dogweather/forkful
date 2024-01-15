---
title:                "לבדיקת קיום תיקייה במחשב"
html_title:           "Haskell: לבדיקת קיום תיקייה במחשב"
simple_title:         "לבדיקת קיום תיקייה במחשב"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## למה

קודם כל, ברוך הבא לוועדת הסופרים החסרים של האסקל! אם אתה כבר מכיר את השפה הזו, אתה יודע שיש לה את הכיף והאתגר של כל שפת תכנות. אם אתה מתחיל, האסקל הוא שפה מצוינת להתחיל. זהו מושג קל לאתחל את ההכנסה הפדים שלך באמצעות השפת תכנות זו. בנוסף, זה נותן לך תחומי ראות וגרף מועילים כדי לקבוע זמנים עורכים חשובים. לכן, למה מישהו יעבור על הקושי לבדוק אם קיים קישור לספרי העיר? אולם, לאחרונה נכבות על הדעת.

## איך לעשות זאת

הבדיקה אם ספריית עיר קיימת באמצעות האסקל היא פשוטה וחכמה יותר. האינדקס בהדף שיטות תנאים בשפת תכנות האסקל של לואה סווטינה יוצא מועיל ומאפשר לוו היה משתמש בפרמטרים כדי לקבוע אם ספריה קיימת או לא.

```haskell
import System.Directory

doesDirectoryExist :: FilePath -> IO Bool

main :: IO ()
main = do
    dir <- doesDirectoryExist "path/to/directory"
    putStrLn $ "Does the directory exist? " ++ show dir
```

פלא שימירות של שם שמופעים ההדלפה של הלוגים בגופן אינטרנטזה, אתה יכול לבדוק שם עם המתודה doesDirectoryExist. ניתן לבחור לבדוק אם קיימת ספריה עם פונקציות נוספות של הספריה System.Directory מדף התיעוד.

## Deep Dive

בנוסף לפונקציה doesDirectoryExist, ישנן כמה פונקציות נוספות שמאפשרות לבדוק אם ספריה קיימת. למשל, הפונקציה getCurrentDirectory מכילה את הפרטים הנוכחיים של הפונקציה שממצת את הפונקציה שפרתית. כמו כן, ניתן ל