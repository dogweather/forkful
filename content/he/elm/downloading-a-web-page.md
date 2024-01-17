---
title:                "הורדת עמוד אינטרנט"
html_title:           "Elm: הורדת עמוד אינטרנט"
simple_title:         "הורדת עמוד אינטרנט"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה ולמה?

הורדת דף אינטרנט היא פעולה שמאפשרת לקוד לטעון ולהציג תוכן מקוון באמצעות תצוגת דפים. תהליך זה חשוב מאוד עבור מפתחי תוכנה, מכיוון שזה מאפשר להציג תוכן מגוון ומגוון רחב של דפי אינטרנט.

## איך לעשות זאת:

```
Elm.Http.send handleHttpResponse (
    Elm.Http.get "https://example.com"
)
```

פלט:

```
handleHttpResponse (Result Result (Data String "Hello, world!") (Redirect "https://example.com"))
```

## חפירה עמוקה:

תהליך זה קיים מזה עשרות שנים ומשמש כיום כמעט בכל אתר אינטרנט. האלטרנטיבות הן שיטות מסורבלות יותר כמו טבלאות, נכתב מחדש עבור כל תצוגה.

בניגוד למספר רב של שפות תכנות המשתמשות בתצוגה סיכום, תהליך זה מיועד לטפל בכמות גדולה של תוכן ולתת יכולות רחבות יותר.

זה מתבצע על ידי שימוש בHTTP תקן, שמאפשר על תקשורת בין שרתים ולקוחות על ידי בעזרת מונחים כמו GET ו POST תטען את הדף ו נתונים חוזרים.

## ראו גם:

- [HTTP על ידי MDN] (https://developer.mozilla.org/en-US/docs/Web/HTTP)
- [להוריד דף אינטרנט עם אלם] (https://package.elm-lang.org/packages/elm/http/latest/Http)