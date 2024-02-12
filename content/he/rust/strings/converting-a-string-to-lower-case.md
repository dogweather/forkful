---
title:                "המרת מחרוזת לאותיות קטנות"
date:                  2024-01-20T17:39:19.311698-07:00
model:                 gpt-4-1106-preview
simple_title:         "המרת מחרוזת לאותיות קטנות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
מה זה המרה של מחרוזת לאותיות קטנות, ולמה זה נחוץ? בפשטות, זה פעולה שמשנה את כל האותיות במחרוזת לאותיות קטנות. תוכניתנים עושים את זה לדברים כמו נורמליזציה של טקסטים לפני השוואות או חיפושים, ולהקלות על ייעול הנתונים.

## How to:
```rust
fn main() {
    let greeting = "Shalom!";
    let lowercased = greeting.to_lowercase();
    println!("{}", lowercased); // שולח לפלט: "shalom!"
}
```
קוד זה יוצר מחרוזת עם אותיות קטנות ממחרוזת נתונה ומדפיס אותה.

## Deep Dive
ההמרה של מחרוזות לאותיות קטנות ב-Rust משתמשת במתודה `to_lowercase()`. היסטורית, כאשר תוכנה התעסקה עם טקסטים באנגלית בלבד, המרת אותיות לקטנות הייתה די פשוטה. אבל עם תמיכה מודרנית ליותר שפות, זה התמודדות עם סוגיות יותר מורכבות כמו אותיות עם סימני ניקוד, אותיות גדולות שמתקבלות מכמה תווים קטנים כשהם במצב קטן, או אותיות ללא צורה גדולה/קטנה ברורה.

רוסט מתמודד עם אותן אתגרים על ידי שימוש במאגר נתונים של Unicode, המרכיב את הפרטים של כל תו לגבי השוואות בין אותיות גדולות וקטנות. חלופה לשיטה זו היא שימוש בעבודה ידנית עם טבלאות המרה ספציפיות לשפה, אבל זה לא מומלץ ויכול להוביל לשגיאות.

## See Also
- [Rust Documentation for `to_lowercase()`](https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase)
- [Unicode Case Folding](https://www.unicode.org/reports/tr44/#CaseFolding)
- [Rust by Example - Strings](https://doc.rust-lang.org/rust-by-example/std/str.html)
