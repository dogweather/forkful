---
title:    "Haskell: יצירת קובץ זמני"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מדוע

יצירת קובץ זמני היא תהליך חשוב בתכנות הייסקל, שמאפשר לנו לנהל נתיבי קבצים ולתפעל בהם על מנת לרכוש נתונים וליצור פלט או מידע חדש. יצירת קובץ זמני היא גם חשובה למניעת תכניות חריגות ובעיות עם נתיבי הקבצים הראשוניים.

## איך לעשות

ראשית, נצטרך לייבא את החבילה המתאימה ליצירת קבץ זמני בהיסקל - `System.IO.Temp`. לאחר מכן, נחיל את הפונקציה `withSystemTempFile`, שתאפשר לנו ליצור קובץ זמני עם שם ונתיב מקרים רנדומליים:

```Haskell
import System.IO.Temp

main = do
  withSystemTempFile "example.txt" $ \tmpFilePath tmpHandle -> do
    putStrLn $ "נוצר קובץ זמני בנתיב: " ++ tmpFilePath
    hPutStrLn tmpHandle "תוכן הקובץ זמני."
```

כפי שניתן לראות, אנו משתמשים בתוך הפונקציה `withSystemTempFile` בפרמטריו `tmpFilePath` ו- `tmpHandle`, שנוצרים באופן אוטומטי עם שמות ונתיבים רנדומליים, המכוונים לכך שנוכל לתפעל בקובץ זמני המיוצר.

התוכן שנכתוב בקובץ הזמני יופיע רק לאחר שנסגר הואנדל `withSystemTempFile`, כך שאם נרצה לעבוד עם הנתונים שהוכנסו לקובץ, ניתן יהיה לעשות זאת מתוך הפונקציה.

## חפירה עמוקה

למרבה המזל, החבילה `System.IO.Temp` מציעה לנו כמה פונקציות נוספות כדי לנהל קבצים זמניים. למשל, ניתן להשתמש ב- `writeSystemTempFile` במקום ב־ `withSystemTempFile`, אשר תאפשר לנו לכתוב את הנתונים ישירות לקובץ זמני:

```Haskell
import System.IO.Temp

main = do
  tmpFilePath <- writeSystemTempFile "example.txt" "תוכן הקובץ זמני."
  putStrLn