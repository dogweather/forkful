---
title:                "כתיבה לשגרת השגיאה התקנית"
html_title:           "Elm: כתיבה לשגרת השגיאה התקנית"
simple_title:         "כתיבה לשגרת השגיאה התקנית"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## מה זה ולמה?

כתיבה לפלט Standard Error היא תהליך שבו משתמשים בתכנות כדי להדפיס מידע על שגיאות והתראות בזמן הרצת התוכנית. זה נחשב לדרך נוחה יותר למצוא שגיאות ולתקן אותם, מאשר להסתמך רק על הטקסט המוצג ברצת התוכנית.

## איך לעשות?

כדי לכתוב לפלט Standard Error ב-Elm, יש להשתמש בפונקציית `Debug.log` ולהעביר כפרמטרים את ההודעה שברצונך להדפיס. נהפך את זה לפעולה תכנותית עם השורה הזו: `Log.error "Oops! Something went wrong!"`. הנה דוגמא של שגיאה שתופיע בפלט Standard Error:

```Elm
Debug.log "Error" (Result.toMaybe (String.toFloat "oops")) // Just 0.0'
```

## נכנס מעומק

התחום של כתיבה לפלט Standard Error התחיל במערכות הפעלה בשנות ה-70 ובמקור נעשה בשימוש בפקודת `stderr` על מנת להדפיס שגיאות והתראות בזמן הרצת התוכנית. אופיו של כתיבה לפלט Standard Error הוא להיות נטולת פייתון על מנת לתת את התחם למידע נכון יותר על השגיאות וההתראות של התוכנית.

לעיתים, יש תחליפים אחרים לכתיבה לפלט Standard Error, כגון התפתחות שגיאות והתראות מרובות, או הדפסה לתצורה גלובלית. בכל מקרה, כתיבה לפלט Standard Error נחשבת לפעולה בסיסית ונמצאת בשימוש מכיוון שיש פתרון נוח ופשוט למציאת שגיאות.

## ראה גם

למידע נוסף על השימוש בפונקציית `Debug.log` ניתן לקרוא כאן: [https://guide.elm-lang.org/debugging/debug.html](https://guide.elm-lang.org/debugging/debug.html)

למידע נוסף על תחומי התכנות והבדיקה, ניתן לבקר באתר הזה: [https://programmersforum.co.il/](https://programmersforum.co.il/)