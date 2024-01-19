---
title:                "יצירת קובץ זמני"
html_title:           "C#: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה ולמה?

הקמת קובץ זמני היא פעולה שבה מייצרים קובץ שמשמש פתרון זמני לאחסון מידע. תכנתים משתמשים בקבצים זמניים לצורך אחסון מידע באופן יעיל ובטוח, ובהנחה שאי לכך חשיבות לשמור אותו לאורך זמן.

## איך ליצור: 

אפשר ליצור קובץ זמני באזרחות באמצעות מנות ה-JavaScript שלנו. הנה דוגמה:

```Haskell
import System.IO.Temp (withSystemTempDirectory)
import System.IO (writeFile)

main :: IO ()
main = do
    withSystemTempDirectory "tempdir" $ \dir -> do
        let filePath = dir ++ "/tempfile"
        writeFile filePath "Hello, World!"
        putStrLn $ "File was written to " ++ filePath
```
זה ייצר קובץ זמני בתיקייה זמנית ויכתוב "Hello, World!" בתוך הקובץ. המיקום של הקובץ הזמני  מודפס למסוף.

## צלילה עמוקה:

### היסטוריה
הטכניקה של הקמת קובץ זמני נמשכת לימים הראשונים של התכנות. ההקמה הראשונה הייתה בראשית שפת העשרים, והיא נמשכת עד היום.

### חלופות
אפשר להשתמש גם במסד נתונים זמני או במספר מערכת זיכרון. אם נדרש מקום אחסון קטן ומהיר, ניתן לשקול שימוש במערכת זיכרון במקום בקובץ זמני.

### פרטי יישום
בהשחלה של סיפרייה `System.IO.Temp`, אנחנו יכולים לשלוט במיקום של הקובץ הזמני ובשם שלו. זה מספק גמישות באחסון זמני של מידע.

## ראה גם:
* תיעוד של [`System.IO.Temp`](http://hackage.haskell.org/package/temporary-1.3/docs/System-IO-Temp.html) ניתן למצוא ב-Hackage.
* מדריך ל[`System.Directory`](https://hackage.haskell.org/package/directory) יכול להיות שימושי במבנה ספריות.