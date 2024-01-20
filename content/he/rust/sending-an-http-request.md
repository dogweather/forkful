---
title:                "שליחת בקשת http"
html_title:           "Bash: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP היא שיטה שבה מחשב מתקשר עם שרת כדי לאחזר או לשלוח נתונים. מתכנתים משתמשים במנגנון זה, המקנה את אופציית הגישה למשאבים מרחוק על Puppet Engine.

## איך עושים:
בדוגמה הבאה, אנו נשלח בקשת GET בעזרת הידגים הראשיים `http_req` של Rust:
```Rust
use http_req::{request, uri::Uri};

let url = "https://www.google.com".parse::<Uri>().unwrap();
let mut writer = Vec::new(); 
let res = request::head(url, &mut writer).unwrap();

println!("Response: {:?}", res);
```
תוצאה דוגמה של הקוד הקטע הזה:
```Rust
Response: Response { status_code: 200, headers: Headers { list: [Header { name: "content-type", value: "text/html; charset=ISO-8859-1" }, Header { name: "date", value: "Wed, 28 Oct 2020 13:29:33 GMT" }, ...] }}
```
## צונם עמוק
### היסטוריה: 
בקרת HTTP בצורה של Requests נוצרה למעשה כדי לספק תשתית לאינטרנט. הדחיפות היא השלב הראשון של כל תקשורת ברשת. 
### חלופות:
תוכל להשתמש בחידודים כמו `Hyper` ו-`reqwest`, אם אתה מחפש ספריות HTTP נוספות מקיפות.
### פרטים על הריצה:
הספריה `http_req` משתמשת במשתנה `Uri` כדי לשמור את נתוני ה URL הממופרטים. הבקשה מתבצעת בעזרת רכיב `request` של lib.
## ראה גם:
[HTTP בקשמה בחירוּץ](https://he.wikipedia.org/wiki/HTTP)
[דוקומנטציה של http_req](https://docs.rs/http_req/0.5.9/http_req/)