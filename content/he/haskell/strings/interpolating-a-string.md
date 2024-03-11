---
date: 2024-01-20 17:50:59.710401-07:00
description: "\u05DE\u05D4 \u05D6\u05D4 \u05D0\u05D9\u05E0\u05D8\u05E8\u05E4\u05D5\
  \u05DC\u05E6\u05D9\u05D4 \u05E9\u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA\
  ? \u05D6\u05D5 \u05D8\u05DB\u05E0\u05D9\u05E7\u05D4 \u05DC\u05D4\u05D8\u05DE\u05E2\
  \u05EA \u05E2\u05E8\u05DB\u05D9\u05DD \u05D1\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA \u05D8\u05E7\u05E1\u05D8 \u05D1\u05E7\u05D5\u05D3. \u05DC\u05DE\
  \u05D4 \u05D6\u05D4 \u05E0\u05D7\u05D5\u05E5? \u05DB\u05D3\u05D9 \u05DC\u05D4\u05E4\
  \u05D5\u05DA \u05D0\u05EA \u05D4\u05EA\u05D5\u05DB\u05E0\u05D4 \u05DC\u05D2\u05DE\
  \u05D9\u05E9\u05D4 \u05D5\u05DB\u05D3\u05D9 \u05DC\u05D4\u05EA\u05D0\u05D9\u05DD\
  \ \u05D8\u05E7\u05E1\u05D8 \u05DC\u05DE\u05E9\u05EA\u05DE\u05E9 \u05D0\u05D5 \u05DE\
  \u05E6\u05D1."
lastmod: '2024-03-11T00:14:12.843120-06:00'
model: gpt-4-1106-preview
summary: "\u05DE\u05D4 \u05D6\u05D4 \u05D0\u05D9\u05E0\u05D8\u05E8\u05E4\u05D5\u05DC\
  \u05E6\u05D9\u05D4 \u05E9\u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA? \u05D6\
  \u05D5 \u05D8\u05DB\u05E0\u05D9\u05E7\u05D4 \u05DC\u05D4\u05D8\u05DE\u05E2\u05EA\
  \ \u05E2\u05E8\u05DB\u05D9\u05DD \u05D1\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05EA \u05D8\u05E7\u05E1\u05D8 \u05D1\u05E7\u05D5\u05D3. \u05DC\u05DE\u05D4\
  \ \u05D6\u05D4 \u05E0\u05D7\u05D5\u05E5? \u05DB\u05D3\u05D9 \u05DC\u05D4\u05E4\u05D5\
  \u05DA \u05D0\u05EA \u05D4\u05EA\u05D5\u05DB\u05E0\u05D4 \u05DC\u05D2\u05DE\u05D9\
  \u05E9\u05D4 \u05D5\u05DB\u05D3\u05D9 \u05DC\u05D4\u05EA\u05D0\u05D9\u05DD \u05D8\
  \u05E7\u05E1\u05D8 \u05DC\u05DE\u05E9\u05EA\u05DE\u05E9 \u05D0\u05D5 \u05DE\u05E6\
  \u05D1."
title: "\u05E9\u05E8\u05D1\u05D5\u05D1 \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## What & Why?
מה זה אינטרפולציה של מחרוזות? זו טכניקה להטמעת ערכים בתוך מחרוזת טקסט בקוד. למה זה נחוץ? כדי להפוך את התוכנה לגמישה וכדי להתאים טקסט למשתמש או מצב.

## How to:
```Haskell
import Text.Printf (printf)

main :: IO ()
main = do
    let name = "דוד"
    let booksCount = 3
    putStrLn $ printf "שלום, %s! יש לך %d ספרים." name booksCount
```
פלט:
```
שלום, דוד! יש לך 3 ספרים.
```

## Deep Dive:
אינטרפולציה של מחרוזות לא הייתה חלק מהשפה המקורית של Haskell, אבל היא הופכת למקובלת יותר בעזרת הספרייה `Text.Printf`. זו ספרייה שמעניקה פונקציונליות דומה לפונקציית `printf` בשפות כמו C. ישנן גם חלופות כמו הפקקיג 'interpolate' ו'formatting' שמציעים גישה עשירה יותר וסינטקס נוח יותר עבור אינטרפולציה.

הללו דרכים לטפל באינטרפולציה:
- `Text.Printf` – גמיש, אבל מבנה סינטקטי מסובך יותר.
- `Data.Text` עם `Data.Text.Lazy.Builder` – מתאים להרכבה יעילה של טקסט מחרוזות רבות.
- חבילות חיצוניות כמו 'interpolate' או 'formatting' – סינטקס נקי יותר, תכונות נוספות.

בחירת הגישה תלויה בצרכים הספציפיים של הפרויקט.

## See Also:
- [Text.Printf documentation](https://hackage.haskell.org/package/base-4.16.1.0/docs/Text-Printf.html)
- [formatting package](https://hackage.haskell.org/package/formatting)
- [interpolate package](https://hackage.haskell.org/package/interpolate)

זוהי התחלה למסע בעולם האינטרפולציה של מחרוזות בהסקל. תהליך למידה עשיר ומלא בפתרונות יעילים מחכה לכם.
