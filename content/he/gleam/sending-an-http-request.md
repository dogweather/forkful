---
title:                "Gleam: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

# למה

משלחת בקשת HTTP היא חלק חיוני מתהליך הפיתוח של תוכניות גלים. היא מאפשרת למשתמש ליצור קשר עם שרת חיצוני ולשלוח ולקבל מידע.

# כיצד לעשות זאת

```Gleam
import gleam/http

// Build the HTTP request
let request =
  http.request("https://www.example.com")
  |> http.get

// Send the request and get the response
let response = http.send(request)

// Print the response body
IO.puts(response.body)

// Output:
// <!DOCTYPE html>
// <html>
//   <head>
//     <title>Example Domain</title>
//     ...
//   </head>
//   ...
// </html>
```

# טביעת קורה עמוקה

שליחת בקשת HTTP יכולה להיות מורכבת יותר מבלוק קוד אחד בלבד. בנוסף לשליחת בקשת פשוטה כמו בדוגמה לעיל, ניתן גם להוסיף הגדרות נוספות כמו שמתאימים, גוף של בקשת POST או להתאים את התוכן של התגובה הנמסרת.

# ראה גם

- [דוקומנטציה רשמית של Gleam לשליחת בקשת HTTP](https://gleam.run/std/http.html#request)
- [ספריית HTTP של גלים](https://github.com/gleam-lang/http)
- [מדריך בשפה ספרדית לשליחת בקשות HTTP עם גלים](https://apuntdev.wordpress.com/2019/05/08/enviando-peticiones-http-con-gleam/)