---
title:                "השוואה בין שני תאריכים"
html_title:           "Arduino: השוואה בין שני תאריכים"
simple_title:         "השוואה בין שני תאריכים"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מה ולמה?
השוואת שני תאריכים היא בסיסית הפעולה של בחינת היחס הזמני ביניהם. מתכנתים מבצעים את זה כדי לארגן, לסנן או לנתח נתונים מבוססי זמן.

## איך לעשות:

בהקשר של השפה של Haskell, נוכל להשתמש בספריית ה- `Data.Time` כדי להשוות שני תאריכים. בהנחה שיש לנו שני `UTCTime`, ההשוואה היא פשוטה:

```Haskell
import Data.Time.Clock

isFirstDateEarlier :: UTCTime -> UTCTime -> Bool
isFirstDateEarlier date1 date2 = date1 < date2
```

בהקשר זה, `isFirstDateEarlier` מחזירה `True` אם התאריך הראשון הגיע לפני השני, ו-`False` מאוחר. הנה פלט דוגמה:

```Haskell
let date1 = UTCTime (fromGregorian 2020 1 1) 0
let date2 = UTCTime (fromGregorian 2021 1 1) 0
isFirstDateEarlier date1 date2 -- Outputs: True
```

## צלילה עמוקה:

כדי להבין את דרך השוואת התאריכים בHaskell, עלינו להעמיק קצת שׁוֹרֶשִׁים לְהִיסטוריה:
1) **ההקשר ההיסטורי** - בבניית Haskell, מתכנתים ראו חשיבות בתמיכה בפונקציונליות נוספת מעבר להשוואת מחרוזות ומספרים. בנוסף המתמחים במהדורות מוצקות נוסף ההיסטוריה, משם Haskell כדוגמה מובהקת למודל מושגי גיחה להשוואה של תאריכים.

2) **חלופות** - ישנן פונקציות אחרות ב- `Data.Time` שהן גם מאפשרות השוואה של תאריכים - `diffUTCTime` ו- `addUTCTime`. עם ההפונקציות האלה אפשר לחשב את ההפרש בין שני תאריכים או להוסיף זמן נתון לתאריך מסוים.

3) **הימומה אם השוואת תאריכים** - בספריית `Data.Time`, הפונקציות של השוואת תאריכים מממשות מנגנון של פונקציות של `Ord`. זה מאפשר להן להשתמש במגבלות של ה- `Ord` מגבלה Comonad המוכר, כי מאפשר למתכנת צעדים מוכנים של השוואה עם `<`, `>`,` ==`, `<=` ו- `>=`.

## ראה גם:

להלן קישורים למקורות מועילים מקוונים שיבהירו הכול:
1. הדוקומנטציה הרשמית של `Data.Time`: https://hackage.haskell.org/package/time-1.5.0.1/docs/Data-Time-Clock-UTCTime.html
2. בלוג גִיּוֹר אגצימ על הגיחה למודלים של זמן: https://yogsototh.github.io/programming/2019/05/06/haskell-time-library-tutorial/
3. קורס מקוון ב- Coursera מתחיל Jeroen Frisius על תכנות פונקציונלי מתרוכב בHaskell: https://www.coursera.org/lecture/principles-of-functional-programming-in-haskell/d73c37a0c