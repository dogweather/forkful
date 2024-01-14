---
title:    "Haskell: קריאת קובץ טקסט"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## למה

קריאת קובץ טקסט היא כלי חשוב למתכנתי Haskell. בכתיבת עקביות, או בטיפול בנתונים מורכבים, קריאת קובץ טקסט היא כלי חיוני שיעזור לנו לעבוד בצורה יעילה ומהירה.

## כיצד לעשות זאת

קוד ה-Haskell הבא מדגים כיצד לקרוא קובץ טקסט באמצעות ספריית Prelude של השפה:

```Haskell
import System.IO

main = do
    handle <- openFile "example.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle
```

על מנת לקרוא את הקובץ, אנו משתמשים בפונקציה openFile כדי לפתוח את הקובץ וליצור ידית שמייצגת אותו. לאחר מכן, אנו משתמשים בפונקציה hGetContents כדי לקרוא את התוכן של הקובץ ולתתו למשתנה בשם contents. לבסוף, אנו מסגרים את הקובץ על ידי שימוש בפונקציה hClose.

הקוד ידפיס את כל התוכן של הקובץ על המסך.

## עיון מעמיק

קריאת קובץ טקסט היא פעולה יחסית פשוטה, אבל לאחריה יש לטפל בקובץ כדי לנקות ולעקוב אחריו כדי להימנע מאיות עקביות בשינויים. בנוסף, אנו יכולים להשתמש בפונקציות נוספות כמו hGetLine ו-hPutStr כדי לקרוא או לכתוב קווים ספציפיים בקובץ.

הנה כמה כתובות מועילות על קריאת קבצים ב-Haskell שיעזרו לך להשתמש בכלי זה בצורה טובה יותר:

- [Hackage: System.IO](https://hackage.haskell.org/package/base-4.14.1.0/docs/System-IO.html) - מסמך המציג את ממשק ה-IO של Haskell, המכיל את הפונקציות הנכונות לשימוש בפתרון הללו.
- [Real World Haskell - Chapter 7: Input and Output](https://www.oreilly.com/library/view/real-world-haskell/9780596800692/ch