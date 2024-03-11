---
date: 2024-01-20 17:38:57.110680-07:00
description: "\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\
  \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA \u05D6\u05D5\
  \ \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05DE\u05E9\u05E0\u05D4 \u05D0\u05EA \u05DB\
  \u05DC \u05D4\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05D4\u05D2\u05D3\u05D5\u05DC\
  \u05D5\u05EA \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\u05D5\u05EA\
  \u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA. \u05EA\u05DB\u05E0\u05EA\u05D9\
  \u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9\
  \ \u05DC\u05D0\u05D7\u05D3 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\u05DE\u05E9\
  \u05DC, \u05DC\u05E6\u05D5\u05E8\u05DA \u05D4\u05E9\u05D5\u05D5\u05D0\u05D4 \u05D0\
  \u05D5 \u05D7\u05D9\u05E4\u05D5\u05E9 \u05D1\u05DC\u05D9\u2026"
lastmod: '2024-03-11T00:14:12.844873-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\
  \u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA \u05D6\u05D5 \u05E4\
  \u05E2\u05D5\u05DC\u05D4 \u05E9\u05DE\u05E9\u05E0\u05D4 \u05D0\u05EA \u05DB\u05DC\
  \ \u05D4\u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05D4\u05D2\u05D3\u05D5\u05DC\u05D5\
  \u05EA \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\u05D5\u05EA\u05D9\
  \u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\
  \ \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\
  \u05D0\u05D7\u05D3 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\u05DE\u05E9\u05DC\
  , \u05DC\u05E6\u05D5\u05E8\u05DA \u05D4\u05E9\u05D5\u05D5\u05D0\u05D4 \u05D0\u05D5\
  \ \u05D7\u05D9\u05E4\u05D5\u05E9 \u05D1\u05DC\u05D9\u2026"
title: "\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\
  \u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
המרת מחרוזת לאותיות קטנות זו פעולה שמשנה את כל האותיות הגדולות במחרוזת לאותיות קטנות. תכנתים עושים את זה כדי לאחד נתונים, למשל, לצורך השוואה או חיפוש בלי רגישות לרישיות.

## איך לעשות:
Haskell כולל מודול שמוכן זמין למרת מחרוזות לאותיות קטנות - `Data.Char`. הנה דוגמה:

```Haskell
import Data.Char (toLower)

lowercaseString :: String -> String
lowercaseString = map toLower

-- דוגמה לשימוש בפונקציה
main :: IO ()
main = putStrLn (lowercaseString "Hello, World!")
-- פלט: hello, world!
```

הפונקציה `toLower` מתירה אות אחת. `map toLower` מפעילה אותה על כל האותיות במחרוזת.

## טבילה עמוקה
הסטנדרט של השפה Haskell מתעדכן עם הזמן, וכך גם התמיכה בעבודה עם טקסט ומחרוזות. `Data.Char` זה חלק מהתקן של Haskell 98.

לחלופין, אפשר להשתמש בספריות צד שלישי כמו `text` או `CaseConversion` ליכולת המרה משופרת ותמיכה בשפות עם קבוצות אותיות יחודיות.

פרטי מימוש: `toLower` ב`Data.Char` מתבצעת באמצעות טבלת גודל ASCII, אך גם כוללת תמונה של אותיות לפי מפת Unicode, כך שתואמת לטווח רחב של שפות ותווים מיוחדים. למרות זאת, יחסיות רישיות יכולים להיות מורכבים בשפות שונות ולכן ייתכנו חריגים.

## ראה גם
- המדריך הרשמי ל`Data.Char`: http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Char.html
- דוקומנטציה על הספרייה `text`: https://hackage.haskell.org/package/text
- פרויקט CaseConversion ב-GitHub: https://github.com/stackbuilders/case-insensitive
- סקירה של הספרייה `CaseConversion`: https://hackage.haskell.org/package/case-conversion
