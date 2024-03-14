---
date: 2024-01-20 17:54:29.070996-07:00
description: "\u05DE\u05D4 \u05D6\u05D4 \u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\
  \u05D1\u05E5 \u05D8\u05E7\u05E1\u05D8? \u05D6\u05D4 \u05EA\u05D4\u05DC\u05D9\u05DA\
  \ \u05E9\u05D1\u05D5 \u05D4\u05EA\u05D5\u05DB\u05E0\u05D9\u05EA \u05E9\u05DC\u05DA\
  \ \u05E7\u05D5\u05E8\u05D0\u05EA \u05DE\u05D9\u05D3\u05E2 \u05DE\u05E7\u05D5\u05D1\
  \u05E5 \u05D8\u05E7\u05E1\u05D8. \u05DC\u05DE\u05D4 \u05D6\u05D4 \u05D7\u05E9\u05D5\
  \u05D1? \u05DB\u05D9 \u05D6\u05D4 \u05DE\u05D0\u05E4\u05E9\u05E8 \u05DC\u05E7\u05D1\
  \u05DC \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D5\u05DC\u05D8\u05E2\u05D5\u05DF\
  \ \u05D0\u05D5\u05EA\u05DD \u05DC\u05EA\u05D5\u05DA \u05D4\u05EA\u05D5\u05DB\u05E0\
  \u05D9\u05EA \u05DC\u05E2\u05D9\u05D1\u05D5\u05D3 \u05D0\u05D5 \u05D0\u05D7\u05E1\
  \u05D5\u05DF."
lastmod: '2024-03-13T22:44:39.441520-06:00'
model: gpt-4-1106-preview
summary: "\u05DE\u05D4 \u05D6\u05D4 \u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\
  \u05E5 \u05D8\u05E7\u05E1\u05D8? \u05D6\u05D4 \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\
  \u05D1\u05D5 \u05D4\u05EA\u05D5\u05DB\u05E0\u05D9\u05EA \u05E9\u05DC\u05DA \u05E7\
  \u05D5\u05E8\u05D0\u05EA \u05DE\u05D9\u05D3\u05E2 \u05DE\u05E7\u05D5\u05D1\u05E5\
  \ \u05D8\u05E7\u05E1\u05D8. \u05DC\u05DE\u05D4 \u05D6\u05D4 \u05D7\u05E9\u05D5\u05D1\
  ? \u05DB\u05D9 \u05D6\u05D4 \u05DE\u05D0\u05E4\u05E9\u05E8 \u05DC\u05E7\u05D1\u05DC\
  \ \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D5\u05DC\u05D8\u05E2\u05D5\u05DF \u05D0\
  \u05D5\u05EA\u05DD \u05DC\u05EA\u05D5\u05DA \u05D4\u05EA\u05D5\u05DB\u05E0\u05D9\
  \u05EA \u05DC\u05E2\u05D9\u05D1\u05D5\u05D3 \u05D0\u05D5 \u05D0\u05D7\u05E1\u05D5\
  \u05DF."
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
---

{{< edit_this_page >}}

## What & Why?
מה זה קריאת קובץ טקסט? זה תהליך שבו התוכנית שלך קוראת מידע מקובץ טקסט. למה זה חשוב? כי זה מאפשר לקבל נתונים ולטעון אותם לתוך התוכנית לעיבוד או אחסון.

## How to:
```Haskell
-- קוד הסקל הבסיסי לקריאת קובץ
import System.IO

-- פונקציה שפותחת וקוראת את הקובץ
readFileContents :: FilePath -> IO String
readFileContents filePath = do
    handle <- openFile filePath ReadMode
    contents <- hGetContents handle
    hClose handle
    return contents

-- ניתן להשתמש ב'interact' הפונקציה ולהדפיס את תוכן הקובץ
main = interact $ \input -> do
    let filePath = "path/to/your/file.txt"
    contents <- readFileContents filePath
    return contents
```
פלט (פיקטיבי):
```
זהו קובץ דוגמה עם תוכן טקסט.
```

## Deep Dive
איך קריאת קובץ התפתחה בהסקל? בעבר, היה צורך בניהול מקורות באופן ידני, דבר שכעת מטופל באופן אוטומטי שהופך את העבודה לפשוטה יותר. קריאת קובץ היא פעולה של קלט/פלט (I/O), משמע שהיא אסינכרונית ועלולה לקרות עיכוב בהמתנה לתוכן.

יש דרכים אחרות לעשות את זה, כמו שימוש ב'`readFile`', שהוא גרסה קצרה ונקייה יותר, אבל יש לו מגבלות בטיפול בשגיאות וניהול משאבים. גם 'bytestring' ו-'text' הם אלטרנטיבות יעילות לטיפול בקבצי טקסט מורכבים.

## See Also
2. [Haskell `bytestring` package](https://hackage.haskell.org/package/bytestring)
3. [Haskell `text` package](https://hackage.haskell.org/package/text)
