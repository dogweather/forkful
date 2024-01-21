---
title:                "הורדת דף אינטרנט"
date:                  2024-01-20T17:44:04.081486-07:00
model:                 gpt-4-1106-preview
simple_title:         "הורדת דף אינטרנט"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה ולמה?
להוריד דף אינטרנט זה פשוט לקבל את התוכן שלו דרך הרשת. תכנתים עושים את זה כדי לנתח נתונים, לבדוק את זמינות האתר, או לאסוף מידע אוטומטית.

## איך לעשות:
בואו נעבור על קטע קוד פשוט שמראה איך להוריד דף אינטרנט ב-Gleam:

```gleam
import gleam/http
import gleam/expected.{Ok, Error}

pub fn download_page(url: String) -> Result(String, String) {
  let response = httpc.send(httpc.build_request(url, "GET"))
  case response {
    Ok(response) -> 
      response.body |> Ok
    Error(error) ->
      error |> Error
  }
}

pub fn main() {
  let url = "http://example.com"
  case download_page(url) {
    Ok(body) -> 
      io.println(body)
    Error(reason) ->
      io.println("Failed to download the page: " ++ reason)
  }
}
```

אם כל הכל עובד כמו שצריך, תראה בקונסול את התוכן של הדף.

## עיון מעמיק
בעבר, להוריד דף אינטרנט היה יותר מסובך. עם התפתחות מכלולי כלים כמו הספרייה `httpc` של Gleam, התהליך הפך להיות יותר אינטואיטיבי. ישנם חלופות אחרות כמו `reqwest` ב-Rust ו-'HTTPClient' ב-Java, אבל `httpc` ב-Gleam הוא בחירה טבעית לתכנתי Erlang ו-Elixir מכיוון שהוא מבוסס על אותם עקרונות ותוכנות.

דפי אינטרנט יכולים להיות מורידים באמצעות פרוטוקולים שונים, אבל הקוד שניתן למעלה משתמש ב-HTTP כי הוא הנפוץ ביותר. HTTPS מספק שכבת אבטחה נוספת והיא חשובה לשימושים אינטראקטיביים.

## ראה גם
- [MDN Web Docs on HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP)
- [Erlang httpc documentation](http://erlang.org/doc/man/httpc.html)