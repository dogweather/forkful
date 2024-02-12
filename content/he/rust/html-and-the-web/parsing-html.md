---
title:                "פיענוח HTML"
aliases:
- /he/rust/parsing-html/
date:                  2024-02-03T19:13:23.123224-07:00
model:                 gpt-4-0125-preview
simple_title:         "פיענוח HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
