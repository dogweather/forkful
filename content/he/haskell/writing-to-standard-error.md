---
title:                "Haskell: כתיבה לפלט השגיאה התקני"
simple_title:         "כתיבה לפלט השגיאה התקני"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## למה:

כתיבה לפלט של שגיאות (standard error) היא כלי חשוב בתכנות ב-Haskell. היא מאפשרת למתכנת לזהות במדויק את המקור של השגיאות ולתקן אותן בקלות. בנוסף, כתיבה לstandard error מאפשרת לנו לצפות בתוצאות רץ של התוכנית בזמן אמת ולקבל מידע חיוני לגבי התכנית שאנחנו מפתחים.

## איך לעשות זאת:

כתיבה לפלט של שגיאות ב-Haskell נעשה באמצעות הפונקציה `hPutStr` או `hPutStrLn` המקבלת כפרמטר את קובץ הפלט שלנו, כפי שניתן לראות בדוגמאות הקוד הבאות:

```Haskell
import System.IO

main = do
    let fileName = "log.txt" -- קובץ הפלט שלנו
    errorFile <- openFile fileName WriteMode
    hPutStr errorFile "משהו רע קרה" -- הוספת הודעת שגיאה לקובץ
    hClose errorFile -- סגירת קובץ הפלט
```

הנתונים שנמסרים לפונקציות כתיבה ל-standard error יופיעו במסך בפורמט הבא: `נתונים בקובץ פלט: <נתונים>`.

## העומק של מצביע השגיאות:

כדי לעשות שימוש יעיל בכתיבה לפלט של שגיאות, חשוב לנצל אותה בתהליך התכנות. כאשר נתבקש לבדוק תכניות באים תקינות ולמצוא אילוצים בתכנות, כתיבת הפלט של שגיאות תסייע לנו לאתר ולהתגבר על הבעיות האלה. בנוסף, כפי שאמרנו למעלה, כתיבה ל-standard error מאפשרת לנו לקבל מידע חיוני בזמן אמת על התכנית שאנחנו מפתחים ולתקן אותה בצורה יעילה ומהירה.

## ראה גם:

* [הוראות הפונקציות hPutStr ו-hPutStrLn בויקיפדיה](https://en.wikibooks.org/wiki/Haskell/Input_and_Output#Error_Outputs)
* [מדריך תכנות ב-Haskell למ