---
title:                "חיפוש והחלפת טקסט"
html_title:           "Elm: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# What & Why?
לחיפוש והחלפת טקסט הוא פעולה קלה ושימושית בתחום תכנות כללי וב־Elm כל כך. התהליך כולל החלפה של מחרוזת טקסט עם מחרוזת אחרת, בכדי לשנות תוכן בקוד או לתקן טעויות. תוכניתנים מבצעים זאת כדי לשפר את קודם ולהקל על התחכמות עם טקסט.

# How to:

בכדי לבצע את הפעולה של החיפוש והחלפת טקסט ב־Elm, נצטרך להשתמש בפונקציה מסוימת בשם `String.replace`. פניותיו של הפונקציה הן מחרוזת טקסט ומחרוזת טקסט נוספת להחלפה. למשל:

```Elm
String.replace "old" "new" "This is the old text" -- return "This is the new text"
```

אנחנו יכולים גם להוסיף מספר למשתנה כדי להחליף את כל המופעים של הטקסט הישן בטקסט חדש, למשל:

```Elm
String.replace "1" "3" "3312" -- return "3332"
```

# Deep Dive:

לפני שמחלקנו התחילו לממש פונקציות לחיפוש והחלפת טקסט ב־Elm, נהגו להשתמש בספריות צד שלישי כדי לבצע פעולה זו. אך כיום, מיותר להשתמש בספריות אלו, מכיוון ש־Elm מספקת פונקציה מובנית ויעילה לחיפוש והחלפת טקסט.

ישנם גם פונקציות נוספות שניתן להשתמש כדי לשנות טקסט בקלות ב־Elm, כגון `String.contains` למציאת מחרוזות מסוימות בתוך מחרוזת מסוימת ו־`String.concat` למציאת חריגות בין מחרוזות כדי להחליף אותם באמצעות `String.replace`.

# See Also:

- המסמך המפורט של פונקציית החיפוש והחלפת טקסט ב־Elm: https://package.elm-lang.org/packages/elm/core/latest/String#replace
- כיצד להשתמש בפונקציות נוספות לשינוי טקסט ב־Elm: https://elmprogramming.com/elm-string-basics.html#replace
- השוואת בין פונקציות החיפוש והחלפת טקסט ב־Elm לבין פונקציות דומות בשפות תכנות אחרות: https://dashbit.co/blog/string-replace-in-elm-vs-other-programming-languages