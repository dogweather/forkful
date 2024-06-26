---
date: 2024-01-26 03:45:55.202826-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA \u05D0\u05EA \u05D6\
  \u05D4: Haskell \u05DE\u05E9\u05EA\u05DE\u05E9 \u05D1\u05E4\u05D5\u05E0\u05E7\u05E6\
  \u05D9\u05D5\u05EA `round`, `ceiling`, `floor`, \u05D5-`truncate` \u05DE\u05D4-`Prelude`\
  \ \u05E2\u05D1\u05D5\u05E8 \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA \u05E2\u05D9\u05D2\
  \u05D5\u05DC."
lastmod: '2024-03-13T22:44:39.405157-06:00'
model: gpt-4-0125-preview
summary: "Haskell \u05DE\u05E9\u05EA\u05DE\u05E9 \u05D1\u05E4\u05D5\u05E0\u05E7\u05E6\
  \u05D9\u05D5\u05EA `round`, `ceiling`, `floor`, \u05D5-`truncate` \u05DE\u05D4-`Prelude`\
  \ \u05E2\u05D1\u05D5\u05E8 \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA \u05E2\u05D9\u05D2\
  \u05D5\u05DC."
title: "\u05E2\u05D9\u05D2\u05D5\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD"
weight: 13
---

## איך לעשות את זה:
Haskell משתמש בפונקציות `round`, `ceiling`, `floor`, ו-`truncate` מה-`Prelude` עבור פעולות עיגול.

```haskell
import Prelude

main :: IO ()
main = do
  let num = 3.567
  print $ round num    -- 4
  print $ ceiling num  -- 4
  print $ floor num    -- 3
  print $ truncate num -- 3
  
  -- עיגול למקום עשרוני מסוים אינו נמצא ב-Prelude.
  -- הנה פונקציה מותאמת:
  let roundTo n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)
  print $ roundTo 1 num -- 3.6
```

## לעומק
מבחינה היסטורית, עיגול הוא חשוב בניתוח נומרי ובמדעי המחשב מכיוון שהוא קריטי למיזום הצטברות שגיאה בחישובים, במיוחד לפני שהתקינו את הייצוגים נקודה צפה עם התקן IEEE 754.

למה לעגל? `round` מביא אותך לשלם הקרוב ביותר—למעלה או למטה. `ceiling` ו-`floor` תמיד יעגלו למעלה או למטה לשלם הקרוב ביותר, בהתאמה, בעוד ש-`truncate` פשוט מוריד את נקודות העשרון.

חלופות לפונקציות אלה עשויות לכלול לוגיקה מותאמת אישית, כמו ה-`roundTo` שלנו, או שאולי תצטרך להשתמש בספריות (כמו Data.Fixed) עבור דרישות מורכבות יותר.

היזהר מתוצאות בלתי צפויות בשל הדרך בה Haskell מטפל במקרים של חצי הדרך ב-`round` (הוא מעגל למספר הזוגי הקרוב ביותר).

## ראה גם
- מסמכי Haskell Prelude עבור פונקציות עיגול: https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html
- הוויקי של Haskell על חשבון נקודה צפה: https://wiki.haskell.org/Floating_point_arithmetic
- התקן IEEE 754-2008 למידע נוסף על אופן הטיפול בנקודה צפה בשפות רבות: https://ieeexplore.ieee.org/document/4610935
