---
title:                "Haskell: יצירת קובץ זמני"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מדוע

יצירת קובץ זמני היא פעולה חיונית בתכנות בשפת Haskell ויש לה מגוון יישומים נרחבים. ניתן להשתמש בקובץ זמני כדי לשמור נתונים שנערכו או להשתמש בהם בתהליכים נפרדים של התכנית.

## איך לעשות זאת

כדי ליצור קובץ זמני בשפת Haskell, ניתן להשתמש בפונקציית 'withSystemTempFile' המאפשרת ליצור קובץ זמני ולבצע פעולות עליו בתוך הפונקציה. לדוגמה:

```Haskell
import System.IO
import System.IO.Temp

main = do
  withSystemTempFile "temp.txt" $ \h temp -> do
    hPutStrLn temp "זהו קובץ זמני"
    hFlush temp
    hGetContents temp >>= putStrLn
```

פלט התוכנית הנ"ל יהיה `זהו קובץ זמני`.

## העומק

קובץ זמני בשפת Haskell מייצג קובץ שמנוי עליו נמסרת העובדה שאין צורך בו לאחר שהתכנית הסתיימה. בשפת Haskell, ניתן להשתמש בקובץ זמני כדי לשמור נתונים שנערכו במהלך התכנית ולאחר מכן למחוק אותו.

פונקציית 'withSystemTempFile' משתמשת בפונקציות 'openTempFile' ו-'openBinaryTempFile' כדי ליצור את הקובץ הזמני. הפונקציה 'openTempFile' מוסיפה ספרה אקראית אחת לשם הקובץ כאשר 'openBinaryTempFile' מוסיפה ספרה אקראית מיוחדת לשם הקובץ כדי למנוע חזרות. לפונקציית 'openTempFile' יש את הספרה הנוספת 'r', מה שמאפשר לקרוא את הקובץ כאשר לפונקציית 'openBinaryTempFile' אין את הספרה הזאת מה שאומר שאין באפשרותך לצפות בתוכן הקובץ עד לשמירת המידע הנכון.

## ראה גם

- [מדריך על קבצים זמניים בשפת Haskell](https://www.haskell.org/cabal/users-guide/developing-packages.html#temp