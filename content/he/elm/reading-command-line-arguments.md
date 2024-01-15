---
title:                "קריאת ארגומנטים מקו הפקודה"
html_title:           "Elm: קריאת ארגומנטים מקו הפקודה"
simple_title:         "קריאת ארגומנטים מקו הפקודה"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

למה: כדי להפעיל תוכניות שלנו מהטרמינל בצורה יעילה ולקבל ערכים מזוהים באופן דינמי, ניתן לקרוא ולעבוד עם ארגומנטים שנכנסו דרך שורת הפקודה.

כיצד לקרוא ארגומנטים מהשורת הפקודה ב-Elm:

```Elm
import Platform

main = Platform.worker
    { init = \_ -> ((), readCommandLineArgs)
    , update = \_ model -> (model, Cmd.none)
    }

type Msg = GotArgs (List String)

readCommandLineArgs : Cmd Msg
readCommandLineArgs =
    Platform.sendToSelf GotArgs

Note.fromText : String -> Maybe Float
```

Deep Dive: ישנם שני אפשרויות לקריאת ארגומנטים מהשורת הפקודה ב-Elm. האופציה הראשונה היא להשתמש בפונקציה `getArgs` מחבילת `Platform.Cmd`, שמחזירה את רשימת הארגומנטים שהתקבלו. האופציה השנייה היא להשתמש בפונקציה `sendToSelf` של `Platform.Cmd`, שאוספת את הארגומנטים בצד השרת ומשתמשת בפונקציית האיניט המוגדרת על ידי המשתמש על מנת לטפל בארגומנטים באופן אסינכרוני.

עמוד הסיום:

ראה גם:

- [Platform.Cmd](https://package.elm-lang.org/packages/elm/core/latest/Platform-Cmd)
- [Platform.worker](https://package.elm-lang.org/packages/elm/core/latest/Platform#worker)
- [Platform.sendToSelf](https://package.elm-lang.org/packages/elm/core/latest/Platform-Cmd#sendToSelf)