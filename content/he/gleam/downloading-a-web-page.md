---
title:                "הורדת דף אינטרנט"
html_title:           "C++: הורדת דף אינטרנט"
simple_title:         "הורדת דף אינטרנט"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה ולמה?
הורדת דף אינטרנט היא פעולה שבה מחשב מתקשר עם שרת רשת כדי לקבל מידע או דף אינטרנט מסוים. מתכנתים עשויים להוריד דפים מקוונים כדי לעבוד על נתונים, לתת שירותים דינמיים או לנתח תוכן מקוון.

## איך להשתמש:
שימוש בשפת Gleam להורדת דף אינטרנט יכול להיראות כך:
```Gleam
import gleam/httpc

pub fn main(args: List(String)) {
  let url = "http://example.com"
  case httpc.get(url) {
    Ok(response) -> 
      io.println(response.body)
    Error(err) -> 
      io.println("Oops! Something went wrong.")
  }
}
```
פלט דוגמה עבור הקוד לעיל:
```
"<html>...content of website...</html>"
```

## צלילה עמוקה
בעבר, הורדת דף אינטרנט הייתה תהליך מורכב המשנה תקנים בין שרתים ודפדפנים. אך ביום וימינו, שפות programing מודרניות כמו Gleam מפשטות את התהליך. אפשרויות חלופיות משלימות יכולות לעבור דרך API המיועד לכך, כמו GraphQL או REST. פרטי המימוש של httpc.get הם פרטים חשופים ומדובר תלוי בקוד של השרת שאליו אנחנו מתקשרים.

## ראו גם
תיעוד Gleam HTTP: https://hexdocs.pm/gleam_http/readme.html
למידת Gleam: https://gleam.run/tour/
חבילת httpc של Gleam: https://hexdocs.pm/gleam_httpc/readme.html