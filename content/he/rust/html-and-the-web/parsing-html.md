---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:23.123224-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05DB\u05D3\u05D9\
  \ \u05DC\u05E4\u05E8\u05E1\u05E8 HTML \u05D1-Rust, \u05DC\u05E8\u05D5\u05D1 \u05EA\
  \u05E9\u05EA\u05DE\u05E9\u05D5 \u05D1-`scraper` crate, \u05D0\u05E9\u05E8 \u05DE\
  \u05E1\u05E4\u05E7 \u05DE\u05DE\u05E9\u05E7 \u05D2\u05D1\u05D5\u05D4 \u05DC\u05E0\
  \u05D9\u05D5\u05D5\u05D8 \u05D5\u05DC\u05E0\u05D9\u05E4\u05D5\u05D9 \u05DE\u05E1\
  \u05DE\u05DB\u05D9 HTML. \u05E8\u05D0\u05E9\u05D9\u05EA, \u05D4\u05D5\u05E1\u05D9\
  \u05E4\u05D5 \u05D0\u05EA `scraper` \u05DC-`Cargo.toml`\u2026"
lastmod: '2024-03-13T22:44:38.983767-06:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05D3\u05D9 \u05DC\u05E4\u05E8\u05E1\u05E8 HTML \u05D1-Rust, \u05DC\
  \u05E8\u05D5\u05D1 \u05EA\u05E9\u05EA\u05DE\u05E9\u05D5 \u05D1-`scraper` crate,\
  \ \u05D0\u05E9\u05E8 \u05DE\u05E1\u05E4\u05E7 \u05DE\u05DE\u05E9\u05E7 \u05D2\u05D1\
  \u05D5\u05D4 \u05DC\u05E0\u05D9\u05D5\u05D5\u05D8 \u05D5\u05DC\u05E0\u05D9\u05E4\
  \u05D5\u05D9 \u05DE\u05E1\u05DE\u05DB\u05D9 HTML."
title: "\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 HTML"
weight: 43
---

## איך לעשות:
כדי לפרסר HTML ב-Rust, לרוב תשתמשו ב-`scraper` crate, אשר מספק ממשק גבוה לניווט ולניפוי מסמכי HTML.

ראשית, הוסיפו את `scraper` ל-`Cargo.toml` שלכם:

```toml
[dependencies]
scraper = "0.12.0"
```

לאחר מכן, הנה דוגמה פשוטה שחולצת כתובות URL של קישורים ממחרוזת HTML נתונה:

```rust
extern crate scraper;

use scraper::{Html, Selector};

fn main() {
    let html = r#"
    <html>
    <body>
        <a href="http://example.com/1">Link 1</a>
        <a href="http://example.com/2">Link 2</a>
    </body>
    </html>
    "#;

    let document = Html::parse_document(html);
    let selector = Selector::parse("a").unwrap();

    for element in document.select(&selector) {
        let link = element.value().attr("href").unwrap();
        println!("מצאתי קישור: {}", link);
    }
}
```

פלט:

```
מצאתי קישור: http://example.com/1
מצאתי קישור: http://example.com/2
```

בדוגמה זו, אנו מפרסרים מסמך HTML פשוט כדי למצוא את כל האלמנטים `<a>` ולחלץ את המאפיינים `href` שלהם, מה שלמעשה מדפיס את כתובות ה-URL של כל הקישורים במסמך. הספרייה `scraper` מפשטת את פרסור HTML ובחירת אלמנטים ספציפיים באמצעות בוררי CSS, מה שהופך אותה לבחירה הנפוצה למשימות גריפת אתרים ב-Rust.
