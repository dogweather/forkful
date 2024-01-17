---
title:                "קריאת פרמטרי שורת הפקודה"
html_title:           "Elm: קריאת פרמטרי שורת הפקודה"
simple_title:         "קריאת פרמטרי שורת הפקודה"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# מה ולמה?
קריאת ארגומנטים משורת הפקודה היא תהליך שבו מקבלים פרמטרים מהמשתמש דרך הPCLI ומשתמשים בהם בתוכניות שלהם. התהליך קריטי לתכנות כיסוי המציאויות ולהתאמת האפליקציות לצרכי המשתמשים שונים.

# איך ?
```Elm
import Platform exposing (worker)

main =
  worker { init = \_ -> ( [], 0 )
         , update = \_ model -> ( [], model + 1 )
         , subscriptions = \_ -> Sub.none
         }
```

בדוגמה זו ניתן לראות כיצד מתבצעת פעולת הקריאה לארגומנטים באמצעות פונקציית עובד. כאשר נקלוט את הערכים שהמשתמש מגדיר בשורת הפקודה, נוסיף אותם לפרמטרים של הפונקציה. לאחר מכן, בעזרת ה-aggregate function, נגדיר את המשתנה המסכם.

```Elm
> elm-make App.elm --args -sunday -location=Israel
0 1 true "Israel"
```

בפלט נמצאים הערכים שנקבלנו משורת הפקודה, כולל את התאריך, היום שבוע ואת המיקום.

# מערכת הפעלה
מערכת ההפעלה היא אחד הפקטורים המשפיעים על אופן הקריאה לארגומנטים. במערכות כמו Windows ו-Linux, ניתן לפענח את המידע מ- PCLI, בעוד ב- MacOS ניתן לגשת ל-Mach-O Binary ולטפל בהנחיות הקוד. וב-macOS ניתן לגשת גם למידע נוסף על ידי גיבור סיפריית ה-Carbon.

# ראה גם
למידע נוסף על קריאת ארגומנטים ועל מערכות הפעלה, ניתן להתייעץ עם המקורות המצורפים:

- [מדריך רשמי של Elm על ניהול ארגומנטים ב-PCLI](https://guide.elm-lang.org/interop/command_line.html)
- [מידע על מערכות הפעלה וקריאת ארגומנטים באלמנטים המגודרים](https://github.com/elm-lang/core/blob/master/docs/Cmd.md)
- [פורום משתמשי Elm עם שאלות ותשובות על קריאת ארגומנטים](https://discourse.elm-lang.org/t/passing-options-to-elm-program/480)