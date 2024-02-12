---
title:                "המרת מחרוזת לאותיות קטנות"
aliases:
- /he/haskell/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:38:57.110680-07:00
model:                 gpt-4-1106-preview
simple_title:         "המרת מחרוזת לאותיות קטנות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/converting-a-string-to-lower-case.md"
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
