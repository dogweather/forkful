---
title:    "Elm: קבלת התאריך הנוכחי"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## How To - כיצד להשתמש בתאריך הנוכחי בתוכנית Elm

השתמש בפונקציית "Time" המובנית ב- Elm כדי להשיג את התאריך הנוכחי במחשב שלך. ניתן לעשות זאת על ידי קריאה לפונקציית "now" ולהשתמש בפונקציה "toUtc" כדי להמיר את התאריך לזמן אוניברסלי.

```Elm
module Main exposing (..)

import Time exposing (utc, toUtc, now)

main =
  toUtc (now utc)
```

כדי להשיג את התאריך בתצורה קריאה יותר, ניתן להשתמש בתוספת "elm/time" שנועד לעזור בעיבוד וניהול זמנים. לדוגמה:

```Elm
module Main exposing (..)

import Time
import Time.Format as Format

main =
  let
    currentLocalTime = Time.now Time.utc
    formattedDate = Format.format "%d/%m/%Y" currentLocalTime
  in
    formattedDate
```

והנה פלט עבור קוד זה:

```Elm
"10/02/2021"
```

## Deep Dive - עומק על קבלת התאריך הנוכחי בשפת Elm

כאשר משתמשים ב- Elm כדי לקבל את התאריך הנוכחי, יש לשים לב לכמה דברים חשובים. פונקצית "now" מחזירה צפוי באייפיי שקורא לה, אך יש לציין שהפונקציה יכולה להחזיר תוצאה שונה בכל פעם שרצים אותה. כמו כן, כאשר משתמשים בפונקצית "toUtc" יש לשים לב שהתאריך מתוחכם לזמן אוניברסלי ולא לזמן מקומי.

## See Also - ראה גם

- [תיעוד לפונקציית "Time" במדריך הרשמי של Elm](https://package.elm-lang.org/packages/elm/time/latest/Time)
- [מדריך לניהול זמנים ב- Elm עם שימוש בתוספת "elm/time"](https://guide.elm-lang.org/effects/time.html)