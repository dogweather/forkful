---
date: 2024-01-20 17:39:19.311698-07:00
description: 'How to: .'
lastmod: '2024-03-13T22:44:38.966028-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u05D4\u05DE\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\u05D0\
  \u05D5\u05EA\u05D9\u05D5\u05EA \u05E7\u05D8\u05E0\u05D5\u05EA"
weight: 4
---

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
