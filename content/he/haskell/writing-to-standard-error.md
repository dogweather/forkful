---
title:                "כתיבה לפלט השגיאה הסטנדרטי"
html_title:           "Arduino: כתיבה לפלט השגיאה הסטנדרטי"
simple_title:         "כתיבה לפלט השגיאה הסטנדרטי"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבה לשגיאה סטנדרטית (standard error) משמשת לדיווח על בעיות ושגיאות במהלך ריצת התכנית. תוכניתיים משתמשים בה כדי להפריד בין הפלט הרגיל לבין הודעות על שגיאות, מה שמאפשר איתור בעיות יותר קל בזמן דיבאג או כאשר התוכנה כבר בשימוש.

## איך לעשות:
Haskell משתמש בפונקציות בסיסיות לכתיבת פלט לשגיאה סטנדרטית. דוגמה:

```Haskell
import System.IO

main :: IO ()
main = do
    hPutStrLn stderr "משהו השתבש!"
```
פלט דוגמה:
```
משהו השתבש!
```

## צלילה עמוקה:
בעבר, כתיבה לשגיאה סטנדרטית הייתה מורכבת יותר ודרשה ידע של ניהול זרמים במערכת ההפעלה. ב-Haskell, `stderr` זהו Handle (מזהה זרם) מובנה המייצג את השגיאה הסטנדרטית. חלופות נפוצות הן כתיבה לקובץ לוג או שימוש במנגנונים מורכבים יותר כמו מונדות לניהול רשומות שגיאה. פנימית, `stderr` ב-Haskell משתמשת ב-APIs של מערכת ההפעלה התחתונה, כך שכתיבה לשגיאה סטנדרטית תתנהג באופן שונה בהתאם לפלטפורמה.

## ראה גם:
- [Haskell System.IO documentation](https://hackage.haskell.org/package/base-4.16.0.0/docs/System-IO.html): מידע על הפונקציות והטיפוסים שעוסקים בקלט/פלט ב-Haskell.
- [The Haskell Programming Language book](http://book.realworldhaskell.org/): ספר שמסביר את יסודות השפה, כולל נושאים של קלט/פלט.
- [Haskell Wiki on IO](https://wiki.haskell.org/IO_inside): הסברים נוספים על תוכנות קלט/פלט ב-Haskell, שבהן השתמשנו בדוגמה.
