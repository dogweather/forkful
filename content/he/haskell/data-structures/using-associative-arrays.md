---
aliases:
- /he/haskell/using-associative-arrays/
changelog:
- 2024-01-30, dogweather, reviewed
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:42.666555-07:00
description: "\u05DE\u05E2\u05E8\u05DB\u05D9\u05DD \u05D0\u05E1\u05D5\u05E6\u05D9\u05D0\
  \u05D8\u05D9\u05D1\u05D9\u05D9\u05DD, \u05D0\u05D5 \u05DE\u05D9\u05DC\u05D5\u05E0\
  \u05D9\u05DD, \u05D1\u05D4\u05D0\u05E1\u05E7\u05DC \u05DE\u05D3\u05D5\u05D1\u05E8\
  \u05D9\u05DD \u05D1\u05E2\u05D9\u05E7\u05E8 \u05E2\u05DC \u05DE\u05D9\u05E4\u05D5\
  \u05D9 \u05DE\u05E4\u05EA\u05D7\u05D5\u05EA \u05DC\u05E2\u05E8\u05DB\u05D9\u05DD\
  \ \u05DC\u05D7\u05D9\u05E4\u05D5\u05E9 \u05DE\u05D4\u05D9\u05E8 \u05D5\u05E0\u05D9\
  \u05D4\u05D5\u05DC \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D9\u05E2\u05D9\u05DC\
  . \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD\
  \ \u05D1\u05D4\u05DD \u05DB\u05D3\u05D9 \u05DC\u05E0\u05D4\u05DC \u05D0\u05D5\u05E1\
  \u05E4\u05D9\u05DD \u05E9\u05DC \u05D6\u05D5\u05D2\u05D5\u05EA\u2026"
lastmod: 2024-02-18 23:08:52.879858
model: gpt-4-0125-preview
summary: "\u05DE\u05E2\u05E8\u05DB\u05D9\u05DD \u05D0\u05E1\u05D5\u05E6\u05D9\u05D0\
  \u05D8\u05D9\u05D1\u05D9\u05D9\u05DD, \u05D0\u05D5 \u05DE\u05D9\u05DC\u05D5\u05E0\
  \u05D9\u05DD, \u05D1\u05D4\u05D0\u05E1\u05E7\u05DC \u05DE\u05D3\u05D5\u05D1\u05E8\
  \u05D9\u05DD \u05D1\u05E2\u05D9\u05E7\u05E8 \u05E2\u05DC \u05DE\u05D9\u05E4\u05D5\
  \u05D9 \u05DE\u05E4\u05EA\u05D7\u05D5\u05EA \u05DC\u05E2\u05E8\u05DB\u05D9\u05DD\
  \ \u05DC\u05D7\u05D9\u05E4\u05D5\u05E9 \u05DE\u05D4\u05D9\u05E8 \u05D5\u05E0\u05D9\
  \u05D4\u05D5\u05DC \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D9\u05E2\u05D9\u05DC\
  . \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD\
  \ \u05D1\u05D4\u05DD \u05DB\u05D3\u05D9 \u05DC\u05E0\u05D4\u05DC \u05D0\u05D5\u05E1\
  \u05E4\u05D9\u05DD \u05E9\u05DC \u05D6\u05D5\u05D2\u05D5\u05EA\u2026"
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E2\u05E8\u05DB\u05D9\u05DD\
  \ \u05D0\u05E1\u05D5\u05E6\u05D9\u05D0\u05D8\u05D9\u05D1\u05D9\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?

מערכים אסוציאטיביים, או מילונים, בהאסקל מדוברים בעיקר על מיפוי מפתחות לערכים לחיפוש מהיר וניהול נתונים יעיל. תכנתים משתמשים בהם כדי לנהל אוספים של זוגות אלמנטים, שבהם החיפוש אחר אלמנט הוא קל ביותר, בהשוואה לרשימות.

## איך לכך:

האסקל לא מכיל מערכים אסוציאטיביים ישירות מהקופסא בדרך כמו שפות אחרות, אך הוא מציע ספרייה סטנדרטית חזקה בשם `Data.Map` לעבודה עם זוגות מפתח-ערך. בואו נקפל את השרוולים ונראה איך להשתמש בהם!

ראשית, ודאו שאתם מייבאים אותה:
```Haskell
import qualified Data.Map as Map
```

יצירת מפה היא פשוטה. בואו ניצור אחת עם כמה שפות תכנות והפרדיגמות שלהן:
```Haskell
let languages = Map.fromList [("Haskell", "Functional"), ("Python", "Imperative"), ("Prolog", "Logical")]
```

כעת, מה לגבי קבלת הפרדיגמה של האסקל?
```Haskell
Map.lookup "Haskell" languages
-- פלט: Just "Functional"
```

הוספת שפה חדשה היא קלה:
```Haskell
let languagesUpdated = Map.insert "Rust" "Systems" languages
```

ומה אם נרצה לרשום את כל השפות? השתמשו ב-`Map.keys`:
```Haskell
Map.keys languagesUpdated
-- פלט: ["Haskell","Python","Prolog","Rust"]
```

כדי לרשום את הפרדיגמות, השתמשו ב-`Map.elems`:
```Haskell
Map.elems languagesUpdated
-- פלט: ["Functional","Imperative","Logical","Systems"]
```

פעולות הבסיס הללו אמורות לכסות את רוב השימושים, אך יש הרבה יותר לחקור ב-`Data.Map`!

## חקירה מעמיקה

המודול `Data.Map` בספרייה הסטנדרטית של האסקל מבוסס על עצים בינאריים מאוזנים, במיוחד עצים מסוג AVL. הבחירה הזו מבטיחה שרוב הפעולות על המפה, כמו הכנסה, מחיקה, וחיפוש, יוכלו להתבצע בזמן O(log n), כאשר n הוא מספר האלמנטים במפה. זו בחירה יעילה למגוון שימושים, אף על פי שהיא לא המהירה ביותר לכל התרחישים.

ישנה גם נימה היסטורית: לפני ש-`Data.Map` הפך לכלי המועדף, תכנתי האסקל לעיתים קרובות השתמשו ברשימות של זוגות כדי לשחזר מערכים אסוציאטיביים. עם זאת, פעולות על מבנים כאלה הן O(n) לחיפוש, מה שהופך את `Data.Map` לשדרוג משמעותי מבחינת ביצועים.

כעת, למרות היעילות והשימושיות של `Data.Map`, הוא לא תמיד הכלי הכי טוב לכל משימה. למשימות עם רגישות גבוהה לביצועים, שבהן זמני חיפוש אפילו של O(log n) הם איטיים מדי, או שבהן המפתחות הם תמיד ערכי מספרים שלמים, מערכים או טבלאות גיבוב (דרך `Data.HashMap`) עשויות להציע ביצועים טובים יותר עם זמני גישה של O(1).

האקוסיסטם של האסקל מאפשר מגוון של מבני נתונים להתאמה לצרכים שונים, ו-`Data.Map` הוא בחירה מצוינת במטרה כללית למערכים אסוציאטיביים, תוך שילוב נוחות שימוש, גמישות, וביצועים.
