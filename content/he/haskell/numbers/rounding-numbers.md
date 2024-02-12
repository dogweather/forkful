---
title:                "עיגול מספרים"
aliases:
- /he/haskell/rounding-numbers/
date:                  2024-01-26T03:45:55.202826-07:00
model:                 gpt-4-0125-preview
simple_title:         "עיגול מספרים"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/rounding-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?

עיגול מספרים מתייחס להתאמתם לשלם הקרוב ביותר או למקום העשרוני המצוין. מתכנתים מעגלים מספרים כדי לשלוט בדיוק, להתאים את הפלטים להצגה בפני המשתמש או להקטין את עלות החישובים עבור פעולות נקודה צפה.

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
