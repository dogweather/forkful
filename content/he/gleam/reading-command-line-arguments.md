---
title:                "קריאה של ארגומנטים משורת הפקודה"
html_title:           "C#: קריאה של ארגומנטים משורת הפקודה"
simple_title:         "קריאה של ארגומנטים משורת הפקודה"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# תכנות בשפת Gleam: קריאת ארגומנטים מהשורה של הפקודה

## מה ולמה?

לקריאת ארגומנטים מהשורה של הפקודה מתייחסים כאשר אנחנו מעבירים פרמטרים או משתנים לתוכנית שלנו בעת הרצתה. זה מאוד שימושי בשל ביצועי סיום, פרמטרים משתנים, וטכניקות שליטה אחרות בתהליך התכנית.

## איך:

קוד ב-Gleam משתמש בחבילת `gleam/otp` על מנת לקרוא את הארגומנטים:
```gleam
import gleam/list
import gleam/otp.{process}

fn main(args: list.List(String)) {
    let args = process.start_link(args)
    case args {
        Ok(val) -> io.format("Success: ~p~n", [val])
        Error(err) -> io.format("Error: ~p~n", [err])
    }
}
```
בריצה של התכנית עם הפרמטר "hello":

```console
$ gleam run main hello
Success: ["hello"]
```

## עומק נוסף:

(1) למרות שקריאת ארגומנטים מהשורה של הפקודה היא רעיון ישן, תמיכתה ב-Gleam חדישה יחסית, מתוך השקפה שמתמקדת בשילוב של שפות ארלאנג ואליקסר. 
(2) חלופות רבות לקריאת ארגומנטים קיימות בשפות אחרות, כולל אופציות מבוססות בניית מילונים או בראייה מפורטת של מידע מהשורה של הפקודה. (3) ב-Gleam, `gleam/otp.process` מאפשר למרות הסינטקס המבונה.

## ראו גם:

- מפרט ה-Gleam OTP: https://hexdocs.pm/gleam_otp/gleam/otp/process.html.
- ספר לימוד שפת תכנות Gleam: https://gleam.run/book/.