---
title:                "שרבוב מחרוזת"
date:                  2024-01-20T17:50:42.691577-07:00
model:                 gpt-4-1106-preview
simple_title:         "שרבוב מחרוזת"

category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)

בעולם התכנות, אינטרפולציה של מחרוזות זה פשוט הטמעה של משתנים או ערכים בתוך טקסט. תכניתנים עושים את זה כדי לקשר נתונים דינמיים עם טקסט קבוע, כמו להדפיס מסרים מותאמים אישית.

## How to (איך לעשות:)

ב-Elm, אתם משלבים אינטרפולציה בעזרת פונקציות. נגיד שכן, יש לכם שם של משתמש ורצים לברך אותו:

```Elm
greetUser : String -> String
greetUser userName =
    "שלום, " ++ userName ++ "!"
```

הרצת הפונקציה עם `"אלי"` תחזיר:

```Elm
"שלום, אלי!"
```

פשוט, נכון?

## Deep Dive (עומק התהום:)

ברוב שפות התכנות, אינטרפולציה היא לעשות את הנתונים לחלק מהמחרוזת בזמן ריצה. ב-Elm, אין לכם מנגנון אינטרפולציה מובנה כמו ב-JavaScript או Python; תצטרכו להשתמש ב-`++` לצירוף מחרוזות. לפעמים זה נראה מסורבל, אבל זה מחזיק אתכם במסלול הטיפוס הבטוח של Elm, ומונע בעיות. אם מחרוזת רבה יותר מדי, שקלו להשתמש ב-`String.concat` או ב-`List.map` עם פונקציה אונומית.

## See Also (ראו גם:)

- התיעוד הרשמי של Elm לקידוד מחרוזות: https://package.elm-lang.org/packages/elm/core/latest/String
- מדריך למיזוג מחרוזות ב-Elm: https://elmprogramming.com/model-update-view.html
- פורום Elm דיסקוס: https://discourse.elm-lang.org/ - אם יש לכם שאלות, זה מקום מעולה לשאול!
