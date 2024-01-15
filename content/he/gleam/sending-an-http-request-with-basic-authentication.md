---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "Gleam: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מדוע

נשתמש באימות בסיסי כדי לאבחן בצורה מאובטחת כאשר מתחברים לשרת האינטרנט שלנו. זה מאפשר לנו לכלול מידע פרטי כחלק מהבקשה ולוודא שרק אנשים מורשים יכולים לקבל את התוכן שלנו.

## איך לעשות זאת

תחילה, נצטרך להתחבר לשרת באמצעות HTTPS כדי להבטיח תקשורת מאובטחת. לאחר מכן, נוסיף את קוד האימות הבסיסי לכותרת הבקשה שלנו, כך שיהיה לנו שם משתמש וסיסמה. להלן דוגמת קוד כיצד לבצע זאת ב־Gleam:

```Gleam
let request_headers =
    [ Header.basic_auth("username", "password") ]
Gleam.http.send (<|
    Request.get("https://www.example.com/")
        |> Request.header(request_headers)
```

הפלט התוצאה יכול לכלול את התוכן של הדף המבוקש מהשרת.

## חקירה מעמיקה

כאשר אנחנו משתמשים באימות בסיסי, המידע הנשלח על ידי המשתמש לכותרת הבקשה מוצפן בבסיס 64. זה מאפשר לנו ליצור חיבור מאובטח בין השרת והמשתמש, כך שאפשר יהיה לשלוט על הגישה לתוכן שלנו ולוודא שרק אנשים מורשים יכולים להתחבר אליו.

## ראה גם

ראה את המקורות הבאים למידע נוסף על כיצד לשלוט על אימות בסיסי בגין החיבור לשרת HTTP:

- דרכון פרוטוקול האינטרנט: https://tools.ietf.org/html/rfc7617
- מדריך לקישורי גישה HTTP: https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization