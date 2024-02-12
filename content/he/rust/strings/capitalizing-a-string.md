---
title:                "הגדלת אותיות במחרוזת"
aliases: - /he/rust/capitalizing-a-string.md
date:                  2024-02-03T19:07:50.211338-07:00
model:                 gpt-4-0125-preview
simple_title:         "הגדלת אותיות במחרוזת"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

הופכים מחרוזת לאותיות רישיות ב-Rust על ידי שינוי המחרוזת כך שהתו הראשון שלה יהיה באות גדולה אם הוא אות, תוך שאר המחרוזת נשארת ללא שינוי. תכנתים לעיתים קרובות מבצעים פעולה זו לצורך עיצוב, כמו להכין מילים לכותרות או להבטיח עקביות בקלט מהמשתמש.

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
