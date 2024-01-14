---
title:                "Haskell: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מדוע

יצירת קובץ זמני היא פעולה חשובה ומועילה למתכנתים בהאסקל. היצירה של קובץ זמני מאפשרת לנו לשמור נתונים אותם אנחנו צריכים לשימוש זמני או לתוכניות טסטים.

## כיצד לבצע

`` `haskell
import System.IO.Temp

main :: IO ()
main = do
  tempFile <- openTempFile "" "myTempFile.txt"
  hPutStrLn tempFile "Hello World!"
  hClose tempFile
`` `

הקוד הזה משתמש בפונקציה `openTempFile` מספרית `System.IO.Temp` כדי ליצור קובץ זמני עם שם המקובל בתוך המשתנה `tempFile`. פונקציית `hPutStrLn` משמשת לכתיבת מחרוזת "Hello World!" לקובץ הזמני. סוף סוף, `hClose` משתמש בכדי לסגור את הקובץ ולשמור אותו.

מה שתראו פה הוא קובץ בשם "myTempFile.txt" עם התוכן "Hello World!" תחת התיקייה הארכיבית "tmp" בתיקיית הנוכחית.

## טעינה עמוקה

יצירת קובץ זמני היא פעולה יעילה כשאתם עובדים עם נתונים גדולים וברצונכם לתבוע ניתוב או לבטל אותם מהמחשב. כמו כן, במתרחש עקב שגיאות בתכנות או בקוד עצמו, יצירת קובץ זמני יכולה להיות תוספת הבטחתית כדי למנוע מחיקה בעייתית של נתונים חשובים.

## ראה גם

- [תיעוד האסקל System.IO.Temp מספרית] (https://hackage.haskell.org/package/base-4.14.0.0/docs/System-IO-Temp.html)
- [מדריך לכתיבת קוד מהיר ויעיל בהאסקל] (https://haskell.org/haskellwiki/Performance)
- [למד האסקל בתמונות - סדרת מדריכים] (https://learnyouahaskell.com/)