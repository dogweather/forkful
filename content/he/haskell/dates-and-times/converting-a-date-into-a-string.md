---
date: 2024-01-20 17:37:12.822110-07:00
description: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05D6\u05D4 \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\
  \u05D1\u05D5 \u05DE\u05E9\u05E0\u05D9\u05DD \u05D0\u05EA \u05E4\u05D5\u05E8\u05DE\
  \u05D8 \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05D8\u05E7\u05E1\u05D8. \u05DE\
  \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA\
  \ \u05D6\u05D4 \u05DC\u05EA\u05E6\u05D5\u05D2\u05D4 \u05E0\u05D5\u05D7\u05D4 \u05DC\
  \u05DE\u05E9\u05EA\u05DE\u05E9 \u05D0\u05D5 \u05DC\u05E2\u05D9\u05D1\u05D5\u05D3\
  \ \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD."
lastmod: '2024-02-25T18:49:37.671738-07:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA \u05D6\u05D4 \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\
  \u05D5 \u05DE\u05E9\u05E0\u05D9\u05DD \u05D0\u05EA \u05E4\u05D5\u05E8\u05DE\u05D8\
  \ \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05D8\u05E7\u05E1\u05D8. \u05DE\u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA \u05D6\
  \u05D4 \u05DC\u05EA\u05E6\u05D5\u05D2\u05D4 \u05E0\u05D5\u05D7\u05D4 \u05DC\u05DE\
  \u05E9\u05EA\u05DE\u05E9 \u05D0\u05D5 \u05DC\u05E2\u05D9\u05D1\u05D5\u05D3 \u05E0\
  \u05EA\u05D5\u05E0\u05D9\u05DD."
title: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
המרת תאריך למחרוזת זה תהליך שבו משנים את פורמט התאריך לטקסט. מתכנתים עושים את זה לתצוגה נוחה למשתמש או לעיבוד נתונים.

## איך לעשות:
בואו נראה כיצד ממירים תאריך למחרוזת בהסקל:

```Haskell
import Data.Time

-- אנו נוצרים תאריך ייצוגי
let sampleDate = UTCTime (fromGregorian 2023 4 1) (secondsToDiffTime 0)

-- פורמטירת התאריך למחרוזת
let dateString = formatTime defaultTimeLocale "%Y-%m-%d" sampleDate

-- הדפסת התאריך המתוך טה"ע
print dateString
```

פלט דוגמה:
```
"2023-04-01"
```

## צלילה לעומק
המרת תאריכים למחרוזות היא חלק מובנה ברוב שפות התכנות. בהסקל, פריסת פורמט התאריך (Date Formatting) נעשית דרך המודול Data.Time, שהוצג בגרסה 1.5 של הפקג"ל time. המרה מאפשרת גמישות בהצגת התאריך למגוון רחב של פורמטים.

חלופות כוללות את שימוש בספריות חיצוניות כמו time-format או הטמעת פונקציה מותאמת אישית לפורמט המיוחד. 

ברמת היישום, חשוב לזכור שההמרה מתבצעת בהתאם לאזור הזמן הנתון והגדרות התרבות (Locale). בתקנים בינלאומיים כמו ISO 8601, פורמט התאריך נפוץ הוא YYYY-MM-DD, אך בפועל יש מגוון רחב של אפשרויות.

## ראה גם
- מסמכי המודול `Data.Time` ב-Hackage: [https://hackage.haskell.org/package/time-1.11.1.2/docs/Data-Time.html](https://hackage.haskell.org/package/time-1.11.1.2/docs/Data-Time.html)
- תיעוד פונקציית `formatTime`: [https://hackage.haskell.org/package/time-1.11.1.2/docs/Data-Time-Format.html#v:formatTime](https://hackage.haskell.org/package/time-1.11.1.2/docs/Data-Time-Format.html#v:formatTime)
