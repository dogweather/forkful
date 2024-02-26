---
date: 2024-01-20 17:40:24.315415-07:00
description: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\
  \u05E0\u05D9 \u05D4\u05D9\u05D0 \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\u05D5\
  \ \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05D9\u05D5\u05E6\u05E8\u05D9\u05DD\
  \ \u05E7\u05D5\u05D1\u05E5 \u05E9\u05D0\u05D9\u05E0\u05D5 \u05E0\u05D5\u05E2\u05D3\
  \ \u05DC\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D0\u05E8\u05D5\u05DA \u05D8\u05D5\u05D5\
  \u05D7. \u05D0\u05E0\u05D7\u05E0\u05D5 \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\
  \u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\u05E4\u05E2\u05D9\u05DC \u05DE\u05D4\u05DC\
  \u05DB\u05D9\u05DD \u05D6\u05DE\u05E0\u05D9\u05D9\u05DD, \u05DC\u05D1\u05D3\u05D5\
  \u05E7 \u05E7\u05D5\u05D3, \u05D0\u05D5 \u05DC\u05D4\u05EA\u05DE\u05D5\u05D3\u05D3\
  \ \u05E2\u05DD \u05DE\u05D9\u05D3\u05E2\u2026"
lastmod: '2024-02-25T18:49:37.684868-07:00'
model: gpt-4-1106-preview
summary: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\u05E0\
  \u05D9 \u05D4\u05D9\u05D0 \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05DE\
  \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05D9\u05D5\u05E6\u05E8\u05D9\u05DD \u05E7\
  \u05D5\u05D1\u05E5 \u05E9\u05D0\u05D9\u05E0\u05D5 \u05E0\u05D5\u05E2\u05D3 \u05DC\
  \u05E9\u05D9\u05DE\u05D5\u05E9 \u05D0\u05E8\u05D5\u05DA \u05D8\u05D5\u05D5\u05D7\
  . \u05D0\u05E0\u05D7\u05E0\u05D5 \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA\
  \ \u05DB\u05D3\u05D9 \u05DC\u05D4\u05E4\u05E2\u05D9\u05DC \u05DE\u05D4\u05DC\u05DB\
  \u05D9\u05DD \u05D6\u05DE\u05E0\u05D9\u05D9\u05DD, \u05DC\u05D1\u05D3\u05D5\u05E7\
  \ \u05E7\u05D5\u05D3, \u05D0\u05D5 \u05DC\u05D4\u05EA\u05DE\u05D5\u05D3\u05D3 \u05E2\
  \u05DD \u05DE\u05D9\u05D3\u05E2\u2026"
title: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D6\u05DE\u05E0\
  \u05D9"
---

{{< edit_this_page >}}

## מה ולמה?
יצירת קובץ זמני היא תהליך שבו מתכנתים יוצרים קובץ שאינו נועד לשימוש ארוך טווח. אנחנו עושים זאת כדי להפעיל מהלכים זמניים, לבדוק קוד, או להתמודד עם מידע שאינו צריך להישאר.

## איך לעשות:
ב-Haskell, ה-library `temporary` מספקת דרך נוחה ליצור ולנהל קבצים זמניים. הנה דוגמה קצרה:

```Haskell
import System.IO.Temp (withSystemTempFile)
import System.IO (hPutStrLn, hClose)

main :: IO ()
main = withSystemTempFile "mytempfile.txt" $ \filePath handle -> do
  -- כתוב לקובץ זמני
  hPutStrLn handle "This is a temporary file content"
  -- סגור את הידית לקובץ
  hClose handle
  -- filePath מכיל את הנתיב לקובץ, ואתה יכול להשתמש בו כאן.
```

לאחר ריצת התוכנית, תיצור ותימחק באופן אוטומטי קובץ זמני.

## עיון מעמיק:
`temporary` library ב-Haskell נוצר במטרה לתת פתרון פשוט ליצירת קבצים ותיקיות זמניים. לפני היותו זמין, התכנתים היו צריכים לנהל את הקבצים האלה ידנית, בסיכון להשאיר קבצים במערכת.

אלטרנטיבות ל-library זו כוללות את יצירת קבצים ידנית בעזרת פקודות מערכת או על ידי שימוש ב-libraries אחרים שמטפלות בניהול משאבים.

מבחינת ביצועים, `temporary` מספקת גישה type-safe וניהול חריג באופן אוטומטי להבטחת מחיקת הקבצים אחרי שימושם.

## ראה גם:
- Haskell `temporary` library documentation: [Hackage - temporary](https://hackage.haskell.org/package/temporary)
