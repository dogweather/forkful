---
title:                "יצירת קובץ זמני"
html_title:           "Haskell: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה ולמה?

יצירת קובץ זמני היא תהליך שמאפשר למתכנתים ליצור קובץ שנמחק באופן אוטומטי לאחר שימוש בו. זהו כלי שימושי לבדיקת פונקציונליות תוכניות או למטרות תיקון באגים.

## איך לעשות?

```Haskell
import System.IO.Temp (withTempFile)

main :: IO ()
main = withTempFile "temp" $ \tempFile handle -> do
  hPutStrLn handle "This is a temporary file."
  hFlush handle
  putStrLn "Temporary file created at:" <> tempFile
```

Output:
```
Temporary file created at: /tmp/temp41982
```

## טביעת שורש

יצירת קובץ זמני נמצאת בשימוש ממזמן בעולם התכנות והיא מתוחכמת על ידי מתכנתים לפתרון בעיות נוספות כמו בעיות תיקונים ואיתור באגים. אפשרויות אחרות ליצירת קובץ זמני כוללות את השימוש בתיקיות במערכת הקבצים או השימוש בתוכניות שליטה כמו shell או Perl.

## היישום בעומק

יצירת קובץ זמני מתבצעת בצורה של ```withTempFile```, שהיא חלק מספריית ההערכים "System.IO.Temp" הכלולה במארח Ardour. תכונה נוספת של המודול היא האפשרות להתיחס לפרוט במסגרת המסלול, מה שאומחד למתכנתים צריבים.

## ראו גם

שימוש בקבצים זמניים: https://wiki.haskell.org/Making a temporary file with System.IO.Temp
מתודולוגיית של חקירה: http://www.arhivtatarstan.com/article/18850