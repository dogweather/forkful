---
title:                "כתיבה לפלט השגיאה הסטנדרטי"
date:                  2024-01-19
simple_title:         "כתיבה לפלט השגיאה הסטנדרטי"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבה ל-stderr (פלט שגיאה סטנדרטי) מאפשרת הפרדה בין הודעות שגיאה לבין פלט רגיל. תוכנית צריכה להשתמש בזה כדי להקל על דיבאגינג ותעדוף המידע.

## איך לעשות:
ב-Elm, כל פלט הוא דרך עצמי ה-DOM או תוך כדי שימוש ב-JavaScript Interop. יש לחבר את Elm ל-JS לכתיבה ל-stderr. אין פונקציה ישירה לזה.

```Elm
port module Main exposing (..)

-- define a port to send error messages
port stderr : String -> Cmd msg

-- use the port in your Elm code
submit : String -> Cmd msg
submit message =
  stderr "This is an error message sent to stderr"

main =
  Html.beginnerProgram { model = model, view = view, update = update }

-- JavaScript side to actually write to stderr
var app = Elm.Main.fullscreen();
app.ports.stderr.subscribe(function(message) {
  console.error(message);
});
```

שים לב: אין output ישיר ב-Elm.

## Deep Dive
בשונה משפות תכנות אחרות, כמו Python או C, Elm עובד בדפדפן ואין לו גישה ישירה ל-stderr. אבן דרך היסטורית הייתה כש-Elm פיתח פיצ'ר של ports לצורך תקשורת עם JS. אלטרנטיבות כוללות שימוש ב-web workers או רשומי console.log אחרים בתוך JS.

## ראה גם
- [Elm Ports Documentation](https://guide.elm-lang.org/interop/ports.html)
- [MDN Web Docs on stderr](https://developer.mozilla.org/en-US/docs/Web/API/console/error)
