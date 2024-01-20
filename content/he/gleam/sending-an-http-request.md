---
title:                "שליחת בקשת http"
html_title:           "Bash: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

# שולחים בקשת HTTP ב-Gleam 

## מה זה ולמה?
שליחת בקשת HTTP היא פעולה שבה מחשב או תוכנה מבקשים מידע משרת על ידי שליחת בקשת HTTP. תכנתים עושים את זה כדי למצוא, לשנות, להוסיף או למחוק נתונים בשרת.

## איך לעשות:
הנה איך אתה שולח HTTP request ב-Gleam.

```Gleam
import gleam/http.{Error}
import gleam/httpc.{get, Client}

fn fetch_data() {
  let client = Client.default()
  let result = get(client, "https://api.example.com/data")
  case result {
    Ok(response) -> io.println(response.body)
    Error(e) -> io.println("Failed to get data: " ++ string.from(e))
  }
}
```

## ביקור עמוק 
במהלך השנים, יציאת בקשת HTTP הפכה לחלק בלתי נפרד מתכנות רבות. בעוד שבשפות אחרות ייתכן שלא היית צריך לדעת כיצד לשלוח בקשת HTTP, ב-Gleam, אנחנו שמים דגש על שליטה מלאה ותחשיב על עצמך. חלופות אחרות כוללות ביבליותיהם של HTTP-ביבליות אחרות, אך Gleam מחזיקה באופציה הכי נוחה לדעתי. 

## לראות גם:
למידע נוסף שאולי יעזור לך להבין:
1. [HTTP ב-VMDN](https://developer.mozilla.org/en-US/docs/Web/HTTP) 
3. [מבוא ל-Gleam](https://gleam.run/getting-started/)