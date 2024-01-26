---
title:                "ניתוח HTML"
date:                  2024-01-20T15:33:54.229003-07:00
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה?

Parsing HTML בעברית פירושו לפרק ולהבין קוד HTML. תכנתים עושים את זה כדי לעבד תוכן מדפי אינטרנט – לקרוא, לשנות, או לאסוף מידע מהם.

## איך לעשות:

שימוש בספריה `scraper`:

```Rust
use scraper::{Html, Selector};

fn main() {
    // קוד HTML לדוגמה
    let html = r#"
        <ul>
            <li>עגבניה</li>
            <li>מלפפון</li>
            <li>פלפל</li>
        </ul>
    "#;

    // פרסינג של הקוד
    let document = Html::parse_document(html);

    // יצירת בורר (selector) לפריטי הרשימה
    let selector = Selector::parse("li").unwrap();

    // איטרציה על פריטים והדפסתם
    for element in document.select(&selector) {
        let text = element.text().collect::<Vec<_>>().join("");
        println!("פריט: {}", text);
    }
}
```
פלט לדוגמה:

```
פריט: עגבניה
פריט: מלפפון
פריט: פלפל
```

## צלילה לעומק

Parsing HTML הוא חלק מאתגר של עיבוד קודים שנוצרו לאינטרנט. בעבר, רוב הפרסינג נעשה בשפות כמו Perl, אבל היום יש ספריות בכל שפה כמעט, כולל Rust. האתגר הוא בקריאה נכונה של מבנים מורכבים וגם בעמידה בתקני האינטרנט השונים. אלטרנטיבות ל`scraper` ב-Rust כוללות את `html5ever` ו`select.rs`. כשאתה כותב קוד לפרסינג, חשוב לבדוק שזה גמיש ויכול להתמודד עם HTML פחות מובנה היטב.

## ראו גם:

- מסמכי הספריה `scraper`: https://docs.rs/scraper
- תיעוד `HTML5` לפיתוח ובדיקות ב-Rust: https://github.com/servo/html5ever
- `select.rs`, ספריה לביטויים כמו ב-jQuery: https://crates.io/crates/select
- מערכת בדיקת ה-RFC ל-HTML כדי להבין את תקני הפרסינג: https://validator.w3.org/
