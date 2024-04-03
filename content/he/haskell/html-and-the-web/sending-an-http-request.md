---
date: 2024-01-20 18:00:30.992927-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: ."
lastmod: '2024-03-13T22:44:39.408352-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP"
weight: 44
---

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
