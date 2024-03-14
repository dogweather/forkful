---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:52.820485-07:00
description: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E7\u05D5\u05D1\u05E5 \u05D8\
  \u05E7\u05E1\u05D8 \u05D1-Haskell \u05D4\u05D9\u05D0 \u05E2\u05DC \u05D9\u05E6\u05D9\
  \u05E8\u05D4 \u05D0\u05D5 \u05E2\u05D3\u05DB\u05D5\u05DF \u05EA\u05D5\u05DB\u05E0\
  \u05D9\u05D5\u05EA \u05D1\u05D0\u05D5\u05E4\u05DF \u05EA\u05DB\u05E0\u05D5\u05EA\
  \u05D9 \u05E2\u05DD \u05EA\u05D5\u05DB\u05DF \u05D8\u05E7\u05E1\u05D8\u05D5\u05D0\
  \u05DC\u05D9. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\
  \u05DD \u05D6\u05D0\u05EA \u05D1\u05DB\u05D3\u05D9 \u05DC\u05E9\u05DE\u05E8 \u05E0\
  \u05EA\u05D5\u05E0\u05D9\u05DD \u05DB\u05DE\u05D5 \u05D4\u05D5\u05D3\u05E2\u05D5\
  \u05EA \u05DC\u05D5\u05D2, \u05E4\u05DC\u05D8 \u05E9\u05DC \u05D9\u05D9\u05E9\u05D5\
  \u05DE\u05D9\u05DD,\u2026"
lastmod: '2024-03-13T22:44:39.443144-06:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\
  \u05E1\u05D8 \u05D1-Haskell \u05D4\u05D9\u05D0 \u05E2\u05DC \u05D9\u05E6\u05D9\u05E8\
  \u05D4 \u05D0\u05D5 \u05E2\u05D3\u05DB\u05D5\u05DF \u05EA\u05D5\u05DB\u05E0\u05D9\
  \u05D5\u05EA \u05D1\u05D0\u05D5\u05E4\u05DF \u05EA\u05DB\u05E0\u05D5\u05EA\u05D9\
  \ \u05E2\u05DD \u05EA\u05D5\u05DB\u05DF \u05D8\u05E7\u05E1\u05D8\u05D5\u05D0\u05DC\
  \u05D9. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD\
  \ \u05D6\u05D0\u05EA \u05D1\u05DB\u05D3\u05D9 \u05DC\u05E9\u05DE\u05E8 \u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD \u05DB\u05DE\u05D5 \u05D4\u05D5\u05D3\u05E2\u05D5\u05EA\
  \ \u05DC\u05D5\u05D2, \u05E4\u05DC\u05D8 \u05E9\u05DC \u05D9\u05D9\u05E9\u05D5\u05DE\
  \u05D9\u05DD,\u2026"
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
---

{{< edit_this_page >}}

## מה ולמה?

כתיבה לקובץ טקסט ב-Haskell היא על יצירה או עדכון תוכניות באופן תכנותי עם תוכן טקסטואלי. מתכנתים עושים זאת בכדי לשמר נתונים כמו הודעות לוג, פלט של יישומים, או לאחסן תוכן שנוצר על ידי המשתמש, דבר המהווה משימה יסודית עבור יישומים הדורשים שמירת נתונים או רישום.

## איך לעשות:

ה-Prelude הסטנדרטי של Haskell מספק תמיכה בסיסית לכתיבה לקבצים באמצעות הפונקציות `writeFile` ו-`appendFile` מהמודול `System.IO`. להלן דוגמה בסיסית של יצירת קובץ חדש (או החלפת קובץ קיים) ולאחר מכן הוספת טקסט לקובץ.

```haskell
import System.IO

-- כתיבה לקובץ, החלפה אם קיים
main :: IO ()
main = do
  writeFile "example.txt" "זוהי שורה ראשונה.\n"
  appendFile "example.txt" "זוהי שורה שנייה.\n"
```

כשאתם מריצים את התוכנית הזו, היא יוצרת (או מנקה) את `example.txt` וכותבת "זוהי שורה ראשונה." ולאחר מכן "זוהי שורה שנייה." בשורה הבאה.

לטיפול מתקדם יותר בקבצים, מתכנתי Haskell לעיתים קרובות פונים לחבילת `text` עבור עיבוד מחרוזות ביעילות ולחבילת `bytestring` לטיפול בנתונים בינאריים. הנה איך להשתמש בחבילת `text` עבור קלט/פלט של קבצים:

ראשית, עליכם להוסיף את `text` לתלות של הפרויקט שלכם. ולאחר מכן, תוכלו להשתמש בה כך:

```haskell
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- כתיבה לקובץ באמצעות חבילת text
main :: IO ()
main = do
  let content = T.pack "השימוש בחבילת text לביצועים טובים יותר.\n"
  TIO.writeFile "textExample.txt" content
  TIO.appendFile "textExample.txt" $ T.pack "הוספת שורה שנייה.\n"
```

בקטע זה, `T.pack` ממיר `String` רגיל לסוג `Text`, היעיל יותר. `TIO.writeFile` ו-`TIO.appendFile` הם השקולות של `text` לכתיבה והוספה לקבצים, בהתאמה.

הרצת הקוד הזה תוביל ליצירת קובץ בשם `textExample.txt` עם שתי שורות של טקסט, הדגמת יכולות יצירה והוספה באמצעות הספרייה המתקדמת `text` לביצועים ויכולות טובים יותר בטיפול בטקסט יוניקוד.
