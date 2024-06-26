---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:50.211338-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05DC\u05D4\u05D5\
  \u05E4\u05DB\u05D9\u05DD \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\u05D5\
  \u05EA \u05E8\u05D0\u05E9\u05D9\u05EA \u05D1-Rust \u05D9\u05E9 \u05DC\u05DA \u05E9\
  \u05E0\u05D9 \u05DE\u05E1\u05DC\u05D5\u05DC\u05D9\u05DD \u05E2\u05D9\u05E7\u05E8\
  \u05D9\u05D9\u05DD: \u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05D9\u05DB\u05D5\u05DC\
  \u05D5\u05EA \u05E9\u05DC \u05E1\u05E4\u05E8\u05D9\u05EA \u05D4\u05EA\u05E7\u05DF\
  \ \u05D0\u05D5 \u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05D0\u05D2\u05E8\u05D9\
  \ \u05D7\u05D5\u05DE\u05E8\u05D9\u05DD \u05E6\u05D3 \u05E9\u05DC\u05D9\u05E9\u05D9\
  \ \u05DC\u05E6\u05D5\u05E8\u05DA \u05E6\u05E8\u05DB\u05D9\u05DD \u05D9\u05D5\u05EA\
  \u05E8\u2026"
lastmod: '2024-03-13T22:44:38.959477-06:00'
model: gpt-4-0125-preview
summary: "\u05DC\u05D4\u05D5\u05E4\u05DB\u05D9\u05DD \u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05EA \u05DC\u05D0\u05D5\u05EA \u05E8\u05D0\u05E9\u05D9\u05EA \u05D1-Rust \u05D9\
  \u05E9 \u05DC\u05DA \u05E9\u05E0\u05D9 \u05DE\u05E1\u05DC\u05D5\u05DC\u05D9\u05DD\
  \ \u05E2\u05D9\u05E7\u05E8\u05D9\u05D9\u05DD."
title: "\u05D4\u05D2\u05D3\u05DC\u05EA \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05D1\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 2
---

## איך לעשות:
להופכים מחרוזת לאות ראשית ב-Rust יש לך שני מסלולים עיקריים: שימוש ביכולות של ספרית התקן או שימוש במאגרי חומרים צד שלישי לצורך צרכים יותר מורכבים או ספציפיים. הנה איך אפשר לעשות שניהם.

### שימוש בספרית התקן של Rust
ספרית התקן של Rust אינה מספקת שיטה ישירה להפיכת מחרוזות לאותיות רישיות, אבל תוכל להשיג זאת על ידי מניפולציה של תווי המחרוזת.

```rust
fn capitalize_first(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

fn main() {
    let my_string = "hello";
    println!("{}", capitalize_first(my_string)); // פלט: Hello
}
```

### שימוש ב-Crate של `heck`
לגישה יותר ישירה, בעיקר כשעובדים בהקשר של עיבוד טקסט גדול יותר,ייתכן שתעדיף להשתמש בספריות צד שלישי כמו `heck`. ה-Crate של `heck` מציע פונקציונליות שונה להמרה של מקרים, כולל דרך פשוטה להפיכת מחרוזות לאותיות רישיות.

ראשית, הוסף את `heck` ל-`Cargo.toml` שלך:

```toml
[dependencies]
heck = "0.4.0"
```

אז, השתמש בו כדי להפוך את המחרוזת שלך לאותיות רישיות:

```rust
extern crate heck; // לא נחוץ בגרסת Rust 2018 או יותר מאוחר
use heck::TitleCase;

fn main() {
    let my_string = "hello world";
    let capitalized = my_string.to_title_case();
    println!("{}", capitalized); // פלט: Hello World
}
```

הערה: השיטה `to_title_case` שמספק `heck` מופכת כל מילה במחרוזת לאות ראשית, שזה עשוי להיות יותר ממה שאתה מחפש אם אתה רק רוצה את התו הראשון של המחרוזת להיות באות ראשית. התאם את השימוש שלך לפי הצרכים הספציפיים שלך.
