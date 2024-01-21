---
title:                "שליחת בקשת HTTP"
date:                  2024-01-20T18:00:30.992927-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/sending-an-http-request.md"
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