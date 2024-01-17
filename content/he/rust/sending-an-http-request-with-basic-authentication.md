---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "Rust: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה זו הקושי הזה ולמה לעבור אותו?
כשמתכנתים רוצים לשלוח בקשת HTTP עם אימות בסיסי, הם משתמשים בנתיב אינטרנט כדי לאכלס את המידע ולגשת אליו. השיטה הזאת מאפשרת לך להגיע למידע מאתרים ושירותים שונים ברשת.

## כיצד לבצע את זה:
```Rust
use reqwest::{Client, Result};
use reqwest::header::HeaderMap;

fn main() -> Result<()> {
    let client = Client::new();
    let mut headers = HeaderMap::new();
    
    // הוספת כותרת מדוך לבקשה
    headers.insert(
        header::AUTHORIZATION,
        header::HeaderValue::from_str("Basic Zm9vOmJhcg==").unwrap(),
    );

    // שליחת בקשה GET עם אימות בסיסי לאתר כלשהו
    let response = client
        .get("https://www.examplewebsite.com")
        .headers(headers)
        .send()?;

    // הפעלת תנאי על הקוד התגובה והדפסת התוצאה
    if response.status().is_success() {
        println!("Response Successful");
    } else {
        println!("{}", response.status());
    }

    Ok(())
}
```

## כיצד זה עובד?
HTTP נוצר כדי לאפשר לאתרים ושירותים שונים לתקשר זה עם זה ולהעביר מידע ביניהם. תהליך האימות הבסיסי מאפשר לך לנצל את התקשורת הנתמכת ברשת HTTP כדי לגשת למידע בצורה מאובטחת יותר.

## ראה גם:
- [מסמכי הנייד של גרם](https://docs.rs/reqwest/0.11.1/reqwest/)
- [הסבר על Authorization אינטרנטי](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)