---
title:                "חילוץ תת-מחרוזות"
html_title:           "Bash: חילוץ תת-מחרוזות"
simple_title:         "חילוץ תת-מחרוזות"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## מה ולמה?
קיחול של תת-מחרוזות הוא הליך שבו מפצלים מחרוזת גדולה לכמה מחרוזות קטנות. מתכנתים משתמשים בזה כדי לנתח את המידע ולטפל בפרטים ספציפיים.

## איך לעשות זאת:
הקוד של Gleam נותן דרך מקובלת  לעבודה עם תת-מחרוזות. נסה את הדוגמה בהמשך:

```Gleam
let name = "Hello, Gleam World!"
let hello = name.slice(start: 0, len: 5)
let world = name.slice(start: 7, len: 11)
```
פלט:
```Gleam
String("Hello")
String("World")
```
## צלילה עמוקה
אין היסטוריה ארוכה של קיצוץ מחרוזות ב-Gleam, אבל הפונקציה slice היא הליבה של המניפולציה הזו. ישנן אלטרנטיבות אחרות ל slice, מהן string.split או תרגול פונקציה חיצונית. המימוש של slice משתמש בהוראות Erlang פנימיות כדי לחתוך את המחרוזת.

## ראה גם
- הדרכה על מחרוזות ב-Gleam: https://gleam.run/book/tour/strings.html
- התיעוד הרשמי של Gleam: https://hexdocs.pm/gleam_stdlib/Gleam.String.html
- בעיה מקורית בגיטהאב על שיפורים למחרוזות: https://github.com/gleam-lang/gleam/issues/634