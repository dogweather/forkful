---
title:                "שליחת בקשת HTTP עם אימות בסיסי"
date:                  2024-01-20T18:02:43.196175-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP עם אימות בסיסי"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP עם אימות בייסיק היא טכניקה להגנה על גישה למשאבי ווב באמצעות שם משתמש וסיסמה. תכניתנים משתמשים בה כדי לוודא שרק משתמשים מורשים יכולים לגשת או לשנות משאבים.

## איך לעשות:
הנה דוגמא של שליחת בקשה עם אימות בייסיק ב-Gleam:

```gleam
import gleam/http
import gleam/http/gleam_uri.{Uri}

fn main() {
  let uri = Uri.parse("http://example.com/resource").unwrap()
  let basic_auth = http.basic_auth("username", "password")
  let request = http.Request(
    method: http.Get,
    uri: uri,
    headers: [basic_auth],
    body: http.NoBody,
  )

  try response = http.send(request)
  case response {
    Ok(response) -> 
      io.println("Success: " ++ response.body)
    Error(error) -> 
      io.println("Error: " ++ error)
  }
}
```
רץ את הקוד ותחזיר תשובה מהשרת. אם הכול הולך כשורה, "Success:" והתוכן יודפסו. אם לא, תראה "Error:" והבעיה.

## עיון מעמיק:
אימות בייסיק היא אחת משיטות האימות הפשוטות והוותיקות ביותר בHTTP. הוא מקודד שם המשתמש והסיסמה ב-base64 ושולח אותם בכותרת הבקשה. עם זאת, ללא שימוש ב-HTTPS, האימות נחשף לידיעת כל המאזינים ברשת.

קיימות גם חלופות יותר בטוחות לאימות בייסיק, כמו אותות OAuth או JWT (JSON Web Token), אשר מאפשרות רמה גבוהה יותר של אבטחה. אך לפעמים מספיק הפשטות והיכולת ליישם מהר הופכים את אימות בייסיק לבחירה הולמת למקרים מסוימים.

ב-HEAD כותרת, אם אינך מבצע HTTP בעת ההעברה, שם המשתמש והסיסמה נשלחים כטקסט רגיל באופן בסיסי. הם מופרדים זה מזה על ידי נקודותיים ואז מקודדים ב-base64.

בגרסה הנוכחית של Gleam, השילוח של בקשה מתבצע לעיתים עם libcurl או ספרייה דומה ברמת המערכת, תלוי בספריה שמשתמשים בה לשליחת בקשות HTTP.

## ראה גם:

- מסמכי Gleam לגבי מודולי HTTP: https://hexdocs.pm/gleam_http/gleam/http/
- מדריך לבסיסי HTTP Authentication של MDN: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- מודול gleam_http: https://github.com/gleam-lang/http
- HTTP קוד המצב שמחזיר שהאימות הצליח או נכשל: https://developer.mozilla.org/en-US/docs/Web/HTTP/Status
