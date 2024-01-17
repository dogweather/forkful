---
title:                "שליחת בקשת http"
html_title:           "Rust: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

# מה ולמה?
שליחת בקשת HTTP היא פעולה נפוצה בתעשיית התכנות המשמשת לשליחת בקשות לשרתים באינטרנט. תכנתה משתמשים בזה כדי לקבל מידע משרתים או לבצע פעולות מסוימות על המידע המוחזר.

# איך לעשות זאת:
Rust מציעה כמה דרכים לשלוח בקשות HTTP. למשל, ניתן להשתמש בספרייה המובנית של Rust, ```std::net::TcpStream```, כדי ליצור חיבור TCP לשרת ולשלוח בקשת HTTP ידנית. נהוג להשתמש כמו כן בספריית שליחת HTTP גנרית כמו Hyper להקל על התקשורת עם השרת.

```rust
// שליחת בקשת GET לכתובת url נתונה
use std::net::TcpStream;

let url = "http://example.com";
let stream = TcpStream::connect(url)?;

stream.write(b"GET / HTTP/1.0\r\n\r\n")?;
let mut response = String::new();
stream.read_to_string(&mut response)?;

println!("התגובה התקבלה: {}", response);
```

# טיפול מעמיק:
טכנולוגיית HTTP כוללת עשרות שנים של התפתחות ושינויים, והיא מופיעה בכמעט כל אתר אינטרנט. כאשר מתכנתים משתמשים בשליחת בקשות HTTP, יש להתאים את הקוד לתקנים הנדרשים על מנת שהתקשורת תצליח. כמו כן, ישנן ספריות נוספות ב-Rust שנועדו לסייע עם תקשורת HTTP, כמו ביצוע בקשות יחסית למידע מסוים באופן אוטומטי.

# למידע נוסף:
למידע נוסף על השתמשת ב-Rust כדי לשלוח בקשות HTTP, ניתן לעיין במקורות הבאים:
- [התיעוד הרשמי של Rust לגבי ספריית TcpStream](https://doc.rust-lang.org/std/net/struct.TcpStream.html)
- [מדריך לספריית Hyper ל-Rust](https://hyper.rs/guide)