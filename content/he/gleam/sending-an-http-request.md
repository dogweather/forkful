---
title:                "שליחת בקשת HTTP"
date:                  2024-01-20T17:59:50.312487-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP היא הליך שבו המחשב שלך מבקש מידע משרת ברשת. תכנתים עושים זאת כדי לשלוף מידע, לשלוח נתונים, או לבצע פעולות מרחוק.

## איך לעשות:
קוד שבו תשלחו בקשת HTTP ב-Gleam עשוי להיראות כך:

```gleam
import gleam/http
import gleam/httpc
import gleam/result.{Result, Ok, Error}

pub fn make_request() -> Result(http.Response, http.Error) {
  httpc.send(http.Request(
    method: http.Get,
    url: "https://example.com",
    headers: [],
    body: nil,
  ))
}

pub fn main() {
  case make_request() {
    Ok(response) -> 
      io.println("Got a response!")
      io.println(response)
    Error(error) ->
      io.println("Oops, there was an error.")
      io.println(error)
  }
}
```

פלט הדוגמה:

```
Got a response!
Response(status: 200, headers: [], body: "Hello, World!")
```

או

```
Oops, there was an error.
Error(ConnectTimeout)
```

## עיון מעמיק
שליחת בקשות HTTP היא חלק אינטגרלי בתכנות כבר מאז אימוץ הפרוטוקול בשנות ה-90. יש חלופות כמו GraphQL ו-WebSocket ליישומים שונים, אבל HTTP נשאר מוביל בזכות פשטותו ורווחיותו. Gleam משתמש בחבילת `httpc` לשליחת בקשות, אבל יש גם חבילות חיצוניות כמו `gleam_http` או `reqwest` שמתווספות יכולות נוספות.

## גם כדאי לראות
- התיעוד של Gleam HTTP: <https://hexdocs.pm/gleam_http/>
- מדריכים ל-Gleam: <https://gleam.run/book>
- רשימת חבילות של Gleam: <https://hex.pm/packages?search=gleam>
