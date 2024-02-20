---
date: 2024-01-20 18:00:30.992927-07:00
description: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05D4\u05D9\
  \u05D0 \u05D3\u05E8\u05DA \u05DC\u05D1\u05E7\u05E9 \u05DE\u05D9\u05D3\u05E2 \u05D0\
  \u05D5 \u05DC\u05D1\u05E6\u05E2 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05DE\u05E9\u05E8\
  \u05EA \u05D1\u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8. \u05DE\u05EA\u05DB\u05E0\
  \u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DC\u05E9\
  \u05DC\u05D9\u05E4\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\u05D0\u05D9\
  \u05E0\u05D8\u05E8\u05D0\u05E7\u05E6\u05D9\u05D4 \u05E2\u05DD API, \u05D5\u05DC\u05E9\
  \u05D5\u05E8\u05EA \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA \u05D0\u05D5\u05D8\u05D5\
  \u05DE\u05D8\u05D9\u05EA \u05D1\u05D9\u05DF \u05E9\u05E8\u05EA\u05D9\u05DD."
lastmod: 2024-02-19 22:04:58.644951
model: gpt-4-1106-preview
summary: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05D4\u05D9\
  \u05D0 \u05D3\u05E8\u05DA \u05DC\u05D1\u05E7\u05E9 \u05DE\u05D9\u05D3\u05E2 \u05D0\
  \u05D5 \u05DC\u05D1\u05E6\u05E2 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05DE\u05E9\u05E8\
  \u05EA \u05D1\u05D0\u05D9\u05E0\u05D8\u05E8\u05E0\u05D8. \u05DE\u05EA\u05DB\u05E0\
  \u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DC\u05E9\
  \u05DC\u05D9\u05E4\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\u05D0\u05D9\
  \u05E0\u05D8\u05E8\u05D0\u05E7\u05E6\u05D9\u05D4 \u05E2\u05DD API, \u05D5\u05DC\u05E9\
  \u05D5\u05E8\u05EA \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA \u05D0\u05D5\u05D8\u05D5\
  \u05DE\u05D8\u05D9\u05EA \u05D1\u05D9\u05DF \u05E9\u05E8\u05EA\u05D9\u05DD."
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP היא דרך לבקש מידע או לבצע פעולה משרת באינטרנט. מתכנתים עושים זאת לשליפת נתונים, לאינטראקציה עם API, ולשורת פעולות אוטומטית בין שרתים.

## איך לעשות:
```Haskell
-- נבצע התקנה של ספריית 'http-conduit' בעזרת cabal:
-- cabal install http-conduit

{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Simple

-- דוגמא לשליחת בקשת GET
main :: IO ()
main = do
    response <- httpLBS "http://httpbin.org/get"
    putStrLn $ "סטטוס: " ++ show (getResponseStatusCode response)
    putStrLn $ "תוכן תשובה: " ++ show (getResponseBody response)
```

הרצת הקוד הזה תחזיר מצב תגובה (כגון 200 לתגובה מוצלחת) ואת התוכן עצמו - בדרך כלל בפורמט JSON או HTML.

## הבטן העמוק:
שליחת בקשת HTTP ב-Haskell לא הייתה תמיד כל כך פשוטה. בעבר, נדרשו יותר שורות קוד והבנה טכנית גבוהה יותר. כיום, ספריות כמו `http-conduit` מספקות ממשק נקי ומקוצר לביצוע בקשות.

באופציות אחרות, ניתן לעשות שימוש בספריות כמו `wreq` או `req`, אשר מציעות תכונות נוספות ושימוש נוח. כל ספרייה מתמקדת בסט שונה של פיצ'רים והתאמה לצרכים ספציפיים.

מבין הפרטים הטכניים של שליחת בקשות HTTP, תספקתי דוגמה בסיסית, אבל זיכרו שיש הרבה יכולות כמו ניהול הדר 'Headers', שימוש בשיטות שונות כמו POST או DELETE, ועבודה עם משתנים של פרמטרים ומשתני סביבה.

## ראה גם:
- [http-conduit on Hackage](https://hackage.haskell.org/package/http-conduit)
- [wreq on GitHub](https://github.com/bos/wreq)
- [req on Hackage](https://hackage.haskell.org/package/req)

המאמרים והמקורות הנ"ל יכולים להרחיב את הידע שלך על שליחת בקשות HTTP בהסקל ובכלל.

גלישה מהנה! 🚀
