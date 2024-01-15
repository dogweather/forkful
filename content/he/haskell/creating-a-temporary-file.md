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

## למה

היצירה של קובץ זמני בהגדרה היא דרך נוחה ובטוחה לשמירה של נתונים זמניים בתוך הקוד שלכם. הקובץ יימחק באופן אוטומטי כאשר התוכנית מסתיימת, מה שמונע שקיפות של נתונים בעת ריצת הקוד שלכם.

## איך לעשות

בואו נתחיל עם דוגמא פשוטה ונראה כיצד ליצור קובץ זמני ב-Haskell.

```Haskell
import System.IO.Temp (withSystemTempFile)
import System.Directory (doesFileExist)

main = do
    withSystemTempFile "temp.txt" $ \tempFilePath tempHandle -> do
        putStrLn $ "Temporary file path: " ++ tempFilePath
        hPutStrLn tempHandle "This is a temporary file."
    exists <- doesFileExist tempFilePath
    putStrLn $ "Does temporary file exist? " ++ show exists
```

בדוגמא זו, אנחנו משתמשים בפונקציה `withSystemTempFile` מהמודול System.IO.Temp כדי ליצור קובץ זמני. למרבה המזל, הפונקציה גם מחזירה נתיב לקובץ הזמני וכן ממשק לכתיבה לקובץ הזמני. בהמשך, אנחנו משתמשים בפונקציה hPutStrLn כדי לכתוב טקסט לקובץ הזמני ובפונקציה doesFileExist מהמודול System.Directory כדי לבדוק האם הקובץ הזמני קיים או לא. כך, בסיום התוכנית, אנחנו מדפיסים את הנתונים שלנו למסך כדי לוודא שכל הקוד עובד כצפוי.

אתם יכולים להשתמש בפונקציה `withSystemTempFile` כדי ליצור קבצים זמניים עם שמות וסיומות שונות באמצעות הפרמטרים הנוספים שלה. רוב המקרים, לקובץ זמני יהיה סיומת .tmp אבל אתם יכולים לשנות זאת.

## חפירה עמוקה

כפי שראינו בדוגמא, פונקציית `withSystemTempFile` מקלה על התהליך של יצירת קובץ זמני. אתם יכולים לראות את פרטי המימוש של הפ