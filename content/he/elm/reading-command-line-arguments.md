---
title:                "קריאה של ארגומנטים משורת הפקודה"
html_title:           "C#: קריאה של ארגומנטים משורת הפקודה"
simple_title:         "קריאה של ארגומנטים משורת הפקודה"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
קריאת ארגומנטים משורת הפקודה זו בעצם אמצעי של התקשרות עם תוכנה בזמן הרצה. מתכנתים משתמשים בזה כדי לאפשר לקוד שלהם להתאים את עצמו למגוון מצבים ולקבל כניסות משתמש במהלך ההרצה.

## כיצד לכתוב:
חשוב לציין שElm אינו תומך בקריאת ארגומנטים מהשורת הפקודה באופן ישיר. אבל, אפשר להיעזר בJavaScript באמצעות שימוש ב-Elm ports.
אז נראה דוגמה:

```Elm
port module Main exposing (..)

port getInput : (String -> msg) -> Sub msg
```
ואתה יכול לשלוח בתוך קובץ JavaScript שמשתמש באפליקצית ה-Elm שלך:

```JavaScript
var app = Elm.Main.init();
process.argv.forEach(function (val, index, array) {
  app.ports.getInput.send(val);
});
```

## Deep Dive
עובדה אינטרסנטית היא שהיכולת לקרוא ארגומנטים משורת הפקודה התפתחה מאוד מוקדם בתולדות התכנות, עוד מימי Unix הראשונים. בחלק מהשפות החדשות יותר, כמו Elm, לא תמצא את הראשית הזו. זו אחת הנחישות של מעצבי השפה, שמגבילים את כמות היכולות שמזמינות חשיפה ישירה למערכת ההפעלה.

איזון מעניין להתמודד עליו הוא Node.js, שמאפשרת גישה פשוטה ויעילה לארגומנטים שנשלחו בשורת הפקודה.

## לצפייה נוספת
- דוקומנטציה של Elm: [https://package.elm-lang.org/packages/elm/core/latest/](https://package.elm-lang.org/packages/elm/core/latest/)
- Elm Ports ספר בתיעוד הרשמי [Ports](https://guide.elm-lang.org/interop/ports.html)
- [Node.js Process.argv](https://nodejs.org/docs/latest/api/process.html#process_process_argv)