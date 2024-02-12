---
title:                "שליחת בקשת HTTP עם אימות בסיסי"
aliases:
- /he/rust/sending-an-http-request-with-basic-authentication/
date:                  2024-01-20T18:02:35.360632-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP עם אימות בסיסי"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP עם אימות בסיסי היא תהליך שבו אנחנו מצרפים שם משתמש וסיסמא לבקשה כדי לאמת את הזהות שלנו מול שרת. תכניתנים משתמשים בזה כדי לקבל גישה למשאבים מוגנים באינטרנט.

## איך לעשות:
ראשית, התקנת חבילת ה-`reqwest` לניהול בקשות HTTP. ב-Cargo.toml שלך:

```toml
[dependencies]
reqwest = "0.11"
base64 = "0.13"
```

אז, קטע קוד לשליחת בקשה עם אימות בסיסי:

```rust
use reqwest;
use base64::{encode};

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let username = "user";
    let password = "pass";
    let auth = encode(format!("{}:{}", username, password));

    let client = reqwest::Client::new();
    let res = client.get("http://example.com/secret-page")
        .header("Authorization", format!("Basic {}", auth))
        .send()
        .await?;

    println!("Status: {}", res.status());
    println!("Headers:\n{:?}", res.headers());
  
    Ok(())
}
```

פלט לדוגמא:

```
Status: 200 OK
Headers:
{
    "content-type": "text/html",
    "content-length": "1549",
    ...
}
```

## הבעיה מקרוב:
אימות בסיסי ב-HTTP הוא יחסית עתיק, דרך פשוטה לסנן גישה למשאבים ברשת. השימוש בו הוא נפוץ במקרים שאין דרישה לרמת אבטחה גבוהה מאוד, כי הוא שולח את השם והסיסמא בתיקוש מאוד פשוט (base64) ולא כבטוח כמו הצפנת TLS/SSL. ישנן אלטרנטיבות כמו אימות מורכב יותר, טוקנים ומערכות OAuth, אבל לאימות בטוח יותר.

## ראו גם:
- [reqwest crate documentation](https://docs.rs/reqwest/)
- [מידע על אימות בסיסי ב-HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication_scheme)
- [מידע על אימות מאובטח יותר OAuth](https://oauth.net/)
