---
title:                "הורדת דף אינטרנט"
aliases: - /he/rust/downloading-a-web-page.md
date:                  2024-01-20T17:45:17.639058-07:00
model:                 gpt-4-1106-preview
simple_title:         "הורדת דף אינטרנט"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה ולמה?
להוריד דף אינטרנט זה לגשת ולשמור תוכן מאתר אינטרנט למחשב האישי שלך. תכנתים עושים זאת כדי לעבד נתונים, לבדוק את האתר שלהם או לשלוף מידע אוטומטית.

## איך לעשות:
הנה קוד פשוט בראסט שמראה איך להוריד דף אינטרנט.

```rust
// ספריות לשימוש
use reqwest; // לבקשות HTTP
use std::error::Error; // לטיפול בשגיאות

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    // כתובת האתר להורדה
    let url = "http://example.com";

    // שליחת בקשת GET
    let response = reqwest::get(url).await?;

    // המרה של התוכן לטקסט
    let content = response.text().await?;

    // הדפסת התוכן
    println!("{}", content);

    Ok(())
}
```
כאשר אתה מריץ את הקוד הזה, התוכן של הדף `http://example.com` יודפס למסוף.

## טבילה עמוקה:
להורדת דף אינטרנט בעבר היינו משתמשים בספריות כמו `curl` או כלים קונסוליים. היום, בראסט, `reqwest` הוא הבחירה הפופולרית כי הוא אסינכרוני ורב עוצמה. אלטרנטיבות כוללות `hyper`, שהיא ספרייה יותר נמוכת-רמה, או `ureq` לבקשות סינכרוניות. להתמודד עם התוכן, יש להמיר אותו לפורמט קריא (לדוגמה, טקסט) ולאחר מכן אפשר לבצע פעולות כמו ניתוח HTML עם `select` או `scraper`.

## ראה גם:
- [reqwest - an ergonomic, batteries-included HTTP Client for Rust](https://github.com/seanmonstar/reqwest)
- [tokio - an asynchronous runtime for the Rust programming language](https://tokio.rs/)
