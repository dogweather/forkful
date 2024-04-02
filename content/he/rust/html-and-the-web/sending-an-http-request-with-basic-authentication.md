---
date: 2024-01-20 18:02:35.360632-07:00
description: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9 \u05D4\u05D9\u05D0\
  \ \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05D0\u05E0\u05D7\u05E0\u05D5\
  \ \u05DE\u05E6\u05E8\u05E4\u05D9\u05DD \u05E9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\
  \ \u05D5\u05E1\u05D9\u05E1\u05DE\u05D0 \u05DC\u05D1\u05E7\u05E9\u05D4 \u05DB\u05D3\
  \u05D9 \u05DC\u05D0\u05DE\u05EA \u05D0\u05EA \u05D4\u05D6\u05D4\u05D5\u05EA \u05E9\
  \u05DC\u05E0\u05D5 \u05DE\u05D5\u05DC \u05E9\u05E8\u05EA. \u05EA\u05DB\u05E0\u05D9\
  \u05EA\u05E0\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D6\
  \u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E7\u05D1\u05DC \u05D2\u05D9\u05E9\u05D4 \u05DC\
  \u05DE\u05E9\u05D0\u05D1\u05D9\u05DD\u2026"
lastmod: '2024-03-13T22:44:38.986951-06:00'
model: gpt-4-1106-preview
summary: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9 \u05D4\u05D9\u05D0\
  \ \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05D0\u05E0\u05D7\u05E0\u05D5\
  \ \u05DE\u05E6\u05E8\u05E4\u05D9\u05DD \u05E9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\
  \ \u05D5\u05E1\u05D9\u05E1\u05DE\u05D0 \u05DC\u05D1\u05E7\u05E9\u05D4 \u05DB\u05D3\
  \u05D9 \u05DC\u05D0\u05DE\u05EA \u05D0\u05EA \u05D4\u05D6\u05D4\u05D5\u05EA \u05E9\
  \u05DC\u05E0\u05D5 \u05DE\u05D5\u05DC \u05E9\u05E8\u05EA. \u05EA\u05DB\u05E0\u05D9\
  \u05EA\u05E0\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D6\
  \u05D4 \u05DB\u05D3\u05D9 \u05DC\u05E7\u05D1\u05DC \u05D2\u05D9\u05E9\u05D4 \u05DC\
  \u05DE\u05E9\u05D0\u05D1\u05D9\u05DD\u2026"
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP \u05E2\u05DD\
  \ \u05D0\u05D9\u05DE\u05D5\u05EA \u05D1\u05E1\u05D9\u05E1\u05D9"
weight: 45
---

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
