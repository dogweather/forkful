---
date: 2024-01-19
description: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC-stderr (\u05E4\u05DC\u05D8 \u05E9\
  \u05D2\u05D9\u05D0\u05D4 \u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9) \u05DE\u05D0\
  \u05E4\u05E9\u05E8\u05EA \u05D4\u05E4\u05E8\u05D3\u05D4 \u05D1\u05D9\u05DF \u05D4\
  \u05D5\u05D3\u05E2\u05D5\u05EA \u05E9\u05D2\u05D9\u05D0\u05D4 \u05DC\u05D1\u05D9\
  \u05DF \u05E4\u05DC\u05D8 \u05E8\u05D2\u05D9\u05DC. \u05EA\u05D5\u05DB\u05E0\u05D9\
  \u05EA \u05E6\u05E8\u05D9\u05DB\u05D4 \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\
  \u05D6\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D4\u05E7\u05DC \u05E2\u05DC \u05D3\u05D9\
  \u05D1\u05D0\u05D2\u05D9\u05E0\u05D2 \u05D5\u05EA\u05E2\u05D3\u05D5\u05E3 \u05D4\
  \u05DE\u05D9\u05D3\u05E2."
lastmod: 2024-02-19 22:04:58.453295
summary: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC-stderr (\u05E4\u05DC\u05D8 \u05E9\u05D2\
  \u05D9\u05D0\u05D4 \u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9) \u05DE\u05D0\u05E4\
  \u05E9\u05E8\u05EA \u05D4\u05E4\u05E8\u05D3\u05D4 \u05D1\u05D9\u05DF \u05D4\u05D5\
  \u05D3\u05E2\u05D5\u05EA \u05E9\u05D2\u05D9\u05D0\u05D4 \u05DC\u05D1\u05D9\u05DF\
  \ \u05E4\u05DC\u05D8 \u05E8\u05D2\u05D9\u05DC. \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA\
  \ \u05E6\u05E8\u05D9\u05DB\u05D4 \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05D6\
  \u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D4\u05E7\u05DC \u05E2\u05DC \u05D3\u05D9\u05D1\
  \u05D0\u05D2\u05D9\u05E0\u05D2 \u05D5\u05EA\u05E2\u05D3\u05D5\u05E3 \u05D4\u05DE\
  \u05D9\u05D3\u05E2."
title: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E4\u05DC\u05D8 \u05D4\u05E9\u05D2\
  \u05D9\u05D0\u05D4 \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9"
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
