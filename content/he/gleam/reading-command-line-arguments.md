---
title:                "קריאת פרמטרים משורת הפקודה"
date:                  2024-01-20T17:56:48.526980-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת פרמטרים משורת הפקודה"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת ארגומנטים משורת הפקודה היא התהליך שבו תוכנית מקבלת פרמטרים מהמשתמש דרך הטרמינל. מתכנתים עושים זאת כדי לאפשר גמישות ושליטה יותר גדולה בהפעלת התוכנית.

## איך לעשות:
ב-Gleam, ניתן לקרוא ארגומנטים משורת הפקודה דרך המודול `gleam/io`. להלן דוגמא פשוטה:

```gleam
import gleam/io

fn main(args: List(String)) {
  case args {
    [] -> 
      io.println("No arguments were passed.")
    [head | _] -> 
      io.println("The first argument is: " ++ head)
  }
}
```

פלט דוגמא אם לא הועברו ארגומנטים:
```
No arguments were passed.
```

פלט דוגמא אם הועבר ארגומנט `"Hello"`:
```
The first argument is: Hello
```

## עיון מעמיק
בעבר, קריאת ארגומנטים משורת הפקודה היתה תחום ששפות כמו C ו-Perl דיברו בו הרבה. גישות מודרניות, כמו ב-Gleam, מנצלות מודולים סטנדרטיים ומספקות API נקי יותר וקל לשימוש. חלופות אולי כוללות ביבליות צד שלישי לפרסינג מתקדם יותר של ארגומנטים, כמו קביעת דגלים ואופציות.

בפלטפורמות מסוימות ובשפות אחרות, קריאת ארגומנטים עשויה להיות מורכבת יותר בשל הבדלים באופן שבו ארגומנטים מנותחים ומועברים לתוכנית. ב-Gleam, הכל עובר ברשימת מחרוזות, מה שמפשט את התהליך.

## ראה גם
- [Gleam getting started guide](https://gleam.run/getting-started/)
- [Command Line Applications in Rust](https://rust-cli.github.io/book/index.html) - למרות שהוא על Rust, יש פרק על קריאת ארגומנטים שעשוי להיות רלוונטי מבחינת תכנון.