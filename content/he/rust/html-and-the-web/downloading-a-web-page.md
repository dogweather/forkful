---
date: 2024-01-20 17:45:17.639058-07:00
description: "\u05DC\u05D4\u05D5\u05E8\u05D9\u05D3 \u05D3\u05E3 \u05D0\u05D9\u05E0\
  \u05D8\u05E8\u05E0\u05D8 \u05D6\u05D4 \u05DC\u05D2\u05E9\u05EA \u05D5\u05DC\u05E9\
  \u05DE\u05D5\u05E8 \u05EA\u05D5\u05DB\u05DF \u05DE\u05D0\u05EA\u05E8 \u05D0\u05D9\
  \u05E0\u05D8\u05E8\u05E0\u05D8 \u05DC\u05DE\u05D7\u05E9\u05D1 \u05D4\u05D0\u05D9\
  \u05E9\u05D9 \u05E9\u05DC\u05DA. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05E2\u05D1\u05D3\
  \ \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\u05D1\u05D3\u05D5\u05E7 \u05D0\u05EA\
  \ \u05D4\u05D0\u05EA\u05E8 \u05E9\u05DC\u05D4\u05DD \u05D0\u05D5 \u05DC\u05E9\u05DC\
  \u05D5\u05E3 \u05DE\u05D9\u05D3\u05E2 \u05D0\u05D5\u05D8\u05D5\u05DE\u05D8\u05D9\
  \u05EA."
lastmod: 2024-02-19 22:04:58.196572
model: gpt-4-1106-preview
summary: "\u05DC\u05D4\u05D5\u05E8\u05D9\u05D3 \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\
  \u05E8\u05E0\u05D8 \u05D6\u05D4 \u05DC\u05D2\u05E9\u05EA \u05D5\u05DC\u05E9\u05DE\
  \u05D5\u05E8 \u05EA\u05D5\u05DB\u05DF \u05DE\u05D0\u05EA\u05E8 \u05D0\u05D9\u05E0\
  \u05D8\u05E8\u05E0\u05D8 \u05DC\u05DE\u05D7\u05E9\u05D1 \u05D4\u05D0\u05D9\u05E9\
  \u05D9 \u05E9\u05DC\u05DA. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05E2\u05D1\u05D3 \u05E0\
  \u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\u05D1\u05D3\u05D5\u05E7 \u05D0\u05EA \u05D4\
  \u05D0\u05EA\u05E8 \u05E9\u05DC\u05D4\u05DD \u05D0\u05D5 \u05DC\u05E9\u05DC\u05D5\
  \u05E3 \u05DE\u05D9\u05D3\u05E2 \u05D0\u05D5\u05D8\u05D5\u05DE\u05D8\u05D9\u05EA\
  ."
title: "\u05D4\u05D5\u05E8\u05D3\u05EA \u05D3\u05E3 \u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05E0\u05D8"
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
