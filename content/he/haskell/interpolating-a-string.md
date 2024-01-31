---
title:                "שרבוב מחרוזת"
date:                  2024-01-20T17:50:59.710401-07:00
model:                 gpt-4-1106-preview
simple_title:         "שרבוב מחרוזת"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/interpolating-a-string.md"
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
