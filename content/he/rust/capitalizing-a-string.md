---
title:                "הפיכת מחרוזת לאותיות ראשונות גדולות"
html_title:           "Rust: הפיכת מחרוזת לאותיות ראשונות גדולות"
simple_title:         "הפיכת מחרוזת לאותיות ראשונות גדולות"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?

הפיכה של מחרוזת לראשי תיבות הוא הגדרה של אות פתיחה של כל מילה בראשי תיבות. מתכנתים משתמשים בה כדי לשפר את קריאות הקוד ולמקד את החשיבה שלם המשתמש על הידע הרלוונטי.

## איך לעשות:

להלן שני דרכים להפוך מחרוזות לראשי תיבות בשפת Rust:

```rust
// ספריית עזר
extern crate unicode_segmentation;
use unicode_segmentation::UnicodeSegmentation;

fn main() {
    let my_string = "rust תכנות בשפת";
    let capitalized = my_string
           .unicode_words()
           .map(|word| word.chars().next().unwrap().to_uppercase() + &word[1..])
           .collect::<Vec<_>>()
           .join(" ");
    println!("{}", capitalized);
}
```

והתוצאה תהיה:
```
Rust תכנות בשפת
```

## Deep Dive:

בראייה ההיסטורית, אנחנו מכירים את הפיכת מחרוזת לראשי תיבות משפת C, אך Rust מציעה גישה אלגנטית יותר לבעיה זו בעזרת ספריית UnicodeSegmentation. עשויות להיות פעמים שהפתרון של Rust לא מתאים, ולכן עשוי להיות שווה לבדוק את ספריות חיצוניות אחרות. מבחינת פרטי המימוש, אנו משתמשים במתודה unicode_words לגזור את המחרוזת למילים, ולאחר מכן מפצלים את כל מילה לתווים ומסדירים את התו הראשון.

## ראו גם:

למידע נוסף על הפיכת מחרוזת לראשי תיבות, ראה את המקורות הבאים:
1. [קוד המקור של UnicodeSegmentation](https://github.com/unicode-rs/unicode-segmentation)
2. [דוקומנטציה רשמית של Rust](https://doc.rust-lang.org/stable/rust-by-example/trait/iter.html)