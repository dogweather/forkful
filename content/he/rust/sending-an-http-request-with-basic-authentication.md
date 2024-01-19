---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "C: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

הסבר ברוסט: שליחת בקשת HTTP עם אימות בסיסי
===================================================

## מה זה ולמה? 

שליחת בקשת HTTP עם אימות בסיסי היא דרך לתקשר עם שרת ה-HTTP תוך מתן פרטי משתמש (שם משתמש וסיסמה) לאמת את הבקשה. זה מצריך כאשר גישה למשאב מסוים מסבירה זיהוי משתמש.

## איך לעשות:
```Rust
use reqwest::{Client, header};
use base64::{encode};

async fn http_request() -> Result<(), Box<dyn std::error::Error>> {
    let client = Client::new();
    let user = "user";
    let pass = "pass";

    let request_auth = format!("{}:{}", user, pass);
    let basic_auth = format!("Basic {}", encode(&request_auth));

    let resp = client.get("https://example.com")
        .header(header::AUTHORIZATION, basic_auth)
        .send()
        .await?;

    println!("{:#?}", resp.text().await?);

    Ok(())
}
```
## צלילה מעמיקה
נוצר בהקשר של מקורות רשת שהיו מנוהלים באופן אנושי, אימות ה-HTTP הבסיסי הוא תהליך פשוט שמבטיח שמשתמש מסוים הוא באמת מי שהוא אומר שהוא. החיסרון הוותיק הוא שזה לא מאוד בטוח - הסיסמאות משדרות בצורה מוצפנת פשוטה, כי מופיעות במחרוזת עם אימות בסיסי.
קיימות חלופות אחרות לאימות, כגון אימות טוקן, OAuth, ו- JWT.
הגרסה הנוכחית של Rust (כמו כל שפות התכנות החדשות) מאפשרת תמיכה בסטנדרטים הללו באמצעות חבילות החיצוניות, כגון 'reqwest' ו-'base64'.

## ראה גם
* [מסמך ה-HTTP של מוזילה ](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
* [מאמרי עזרה נוספים על Rust](https://www.rust-lang.org/community)
* [מאמרים בנושא אימות HTTP](https://www.digitalocean.com/community/tags/http-authentication)