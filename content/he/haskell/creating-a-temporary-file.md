---
title:    "Haskell: יצירת קובץ זמני"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## למה

יצירת קובץ זמני בפעולת חשיפת קובץ זמני, מאפשרת למפתחים לכתוב קוד בקלות יותר ובצורה מאורגנת יותר. קבצים זמניים משמשים כדי לשמור תאריכים או נתונים שאינם נדרשים למשך זמן רב וכתוצאה מכך יש בהם חיי נדרשות.

## איך לעשות זאת

כדי ליצור קובץ זמני בHaskell ניתן להשתמש בפעולת `withTempFile` מהספרייה `System.IO.Temp`. פעולה זו תיצור קובץ זמני, תמלא אותו עם התוכן הנדרש ותבצע את הפעולה הנתונה על הקובץ. כדי לסגור את הקובץ ולמחוקו בסופו של דבר, יש להשתמש בפעולת `withSystemTempFile`. ניתן לראות דוגמא לשימוש בפקודות האלה בקטע הקוד המצורף.

```Haskell
import System.IO.Temp (withTempFile, withSystemTempFile)
import System.IO (hGetContents)

-- פעולה זו תעודכן קובץ ותחשוף אותו לאורך הקוד
updateAndExposeFile :: FilePath -> IO ()
updateAndExposeFile path = do
    withTempFile "myTempFile" $ \tempFilePath tempHandle -> do
        -- קלט ועריכת הקובץ הזמני
        fileContents <- hGetContents tempHandle
        let updatedContents = "New data"
        hPutStr tempHandle updatedContents

        -- מעתיקים את הנתונים לקובץ המקורי
        fileContents <- hGetContents tempHandle
        writeFile path fileContents

-- פעולה זו תוחק את ההפקודה הקודמת ותסגור את הקובץ בסוף
updateAndExposeFileWithSystemTemp :: FilePath -> IO ()
updateAndExposeFileWithSystemTemp path = do
    withSystemTempFile "myTempFile" $ \tempFilePath tempHandle -> do
        -- קלט ועריכת הקובץ הזמני
        fileContents <- hGetContents tempHandle
        let updatedContents = "New data"
        hPutStr tempHandle updatedContents

        -- מעתיקים את הנתונים לקובץ המקורי
        fileContents <- hGetContents tempHandle
        writeFile path fileContents
```

## להכנס לעומק

פעולת `withTempFile` תגדיר קובץ זמני באופן אוטומטי עם שם ונתיב אקראיים, אבל ניתן ל