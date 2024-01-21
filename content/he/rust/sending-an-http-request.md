---
title:                "שליחת בקשת HTTP"
date:                  2024-01-20T18:00:53.675494-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מה ולמה?
HTTP זה מעין דואר בין האפליקציה שלך לשרת. מתכנתים שולחים בקשות HTTP כדי לקבל נתונים, לשלוח נתונים, לעדכן או למחוק - זה איך אתה מתקשר עם האינטרנט.

## איך לעשות:
הנה דוגמת קוד ששולחת בקשת GET בפשטות:

```rust
use reqwest; // תוסף לטיפול בבקשות HTTP

#[tokio::main] // מאפשרת תכנות אסינכרוני
async fn main() -> Result<(), reqwest::Error> {
    // שליחת בקשת GET
    let response = reqwest::get("https://www.example.com").await?;
    
    // הדפסת גוף התגובה כמחרוזת
    println!("Response text: {}", response.text().await?);

    Ok(())
}
```

תוצאת דוגמה:

```
Response text: <html>...
```

## עיון נוסף
HTTP נולד בשנות ה-90 והתפתח מאז. ישנם אלטרנטיבות ל-reqwest, כמו hyper ו-isahc, אבל reqwest הוא הפופולרי בגלל נוחות השימוש. חשוב להבין מהן וואריאציות כמו GET, POST, PUT ו-DELETE ומתי להשתמש בכל אחת.

## ראה גם
- [Reqwest](https://docs.rs/reqwest/)
- [HTTP מדריך למתחילים](https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP)
- [אסינכרון בראסט](https://rust-lang.github.io/async-book/)
- [Hyper](https://hyper.rs/) - יישום HTTP מתקדם יותר לראסט