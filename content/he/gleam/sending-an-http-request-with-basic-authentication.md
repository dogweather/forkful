---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "C: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה ולמה?

שליחת בקשת HTTP עם אימות בסיסי היא תהליך שבו תכנית נותנת מידע אימות לשרת. התכניתות משתמשות בכך כדי להבטיח שהן מתקשרות עם השרת הנכון ובאופן בטוח.

## כיצד ל:

הנה דוגמה של שליחת בקשת HTTP עם אימות בסיסי באמצעות שפת התכנות Gleam:

```Gleam
import gleam/http.{Get, basic_auth}
import gleam/uri.Uri
import gleam/httpc

let request = Get
  |> basic_auth("username", "password")
  |> httpc.request(Uri.parse("http://example.com")?)
let response = httpc.send(request)

assert Ok(response) = response
```
הקוד הזה משלח בקשת GET לכתובת `"http://example.com"` באמצעות שם המשתמש `"username"` והסיסמה `"password"` כאימות בסיסי.

## צלילה עמוקה

אימות בסיסי ב-HTTP הוא שיטה יחסית ישנה שנוצרה בשנות ה-90. היא מאוד משובשת ולא מאוד בטוחה, אך היא עדיין מצויה בשימוש. ישנן חלופות יותר מודרניות ובטוחות, כמו OAuth ו-Token Based Authentication. כאשר אתה שולח בקשת HTTP עם אימות בסיסי, השם משתמש והסיסמה שלך מאוחדים למחרוזת אחת, שמועברת בבקשת ה-HTTP כחלק מכותרת ה-Authorization.

## ראה גם

[המסמך המקורי של Basic Authentication Schema](https://tools.ietf.org/html/rfc7617)

[מסמך Gleam HTTP](https://hexdocs.pm/gleam_http/gleam/http/)