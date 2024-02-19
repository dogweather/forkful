---
aliases:
- /he/rust/parsing-html/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:23.123224-07:00
description: "\u05E4\u05E8\u05E1\u05D5\u05E8 HTML \u05D1-Rust \u05D4\u05D5\u05D0 \u05E2\
  \u05DC \u05D7\u05D9\u05DC\u05D5\u05E5 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05DE\
  \u05DE\u05E1\u05DE\u05DB\u05D9 HTML, \u05DE\u05D4 \u05E9\u05D7\u05D9\u05D5\u05E0\
  \u05D9 \u05DC\u05D8\u05D5\u05D1\u05EA \u05D2\u05E8\u05D9\u05E4\u05EA \u05D0\u05EA\
  \u05E8\u05D9\u05DD, \u05D7\u05D9\u05DC\u05D5\u05E5 \u05E0\u05EA\u05D5\u05E0\u05D9\
  \u05DD, \u05D0\u05D5 \u05D1\u05E0\u05D9\u05D9\u05EA \u05D6\u05D7\u05DC\u05E0\u05D9\
  \ \u05D0\u05EA\u05E8\u05D9\u05DD. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D0\u05D5\u05D8\
  \u05DE\u05D8 \u05D0\u05EA \u05D0\u05D9\u05E1\u05D5\u05E3\u2026"
lastmod: 2024-02-18 23:08:52.612399
model: gpt-4-0125-preview
summary: "\u05E4\u05E8\u05E1\u05D5\u05E8 HTML \u05D1-Rust \u05D4\u05D5\u05D0 \u05E2\
  \u05DC \u05D7\u05D9\u05DC\u05D5\u05E5 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05DE\
  \u05DE\u05E1\u05DE\u05DB\u05D9 HTML, \u05DE\u05D4 \u05E9\u05D7\u05D9\u05D5\u05E0\
  \u05D9 \u05DC\u05D8\u05D5\u05D1\u05EA \u05D2\u05E8\u05D9\u05E4\u05EA \u05D0\u05EA\
  \u05E8\u05D9\u05DD, \u05D7\u05D9\u05DC\u05D5\u05E5 \u05E0\u05EA\u05D5\u05E0\u05D9\
  \u05DD, \u05D0\u05D5 \u05D1\u05E0\u05D9\u05D9\u05EA \u05D6\u05D7\u05DC\u05E0\u05D9\
  \ \u05D0\u05EA\u05E8\u05D9\u05DD. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D0\u05D5\u05D8\
  \u05DE\u05D8 \u05D0\u05EA \u05D0\u05D9\u05E1\u05D5\u05E3\u2026"
title: "\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 HTML"
---

{{< edit_this_page >}}

## מה ולמה?

פרסור HTML ב-Rust הוא על חילוץ נתונים ממסמכי HTML, מה שחיוני לטובת גריפת אתרים, חילוץ נתונים, או בניית זחלני אתרים. תכנתים עושים זאת כדי לאוטמט את איסוף המידע מהאינטרנט, לנתח תוכן אינטרנטי או להעביר תוכן מפלטפורמה אחת לאחרת.

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
