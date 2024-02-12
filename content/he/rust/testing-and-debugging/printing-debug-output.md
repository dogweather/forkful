---
title:                "הדפסת פלט לניפוי באגים"
aliases:
- he/rust/printing-debug-output.md
date:                  2024-01-20T17:53:25.872402-07:00
model:                 gpt-4-1106-preview
simple_title:         "הדפסת פלט לניפוי באגים"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## מה ולמה?
דיבאג (Debug) הוא הדפסת בדיקות בקוד כדי להבין מה קורה במהלך ריצת התוכנית. מתכנתים עושים זאת כדי לאתר באגים, לפקח על זרימת התכנית ולבדוק תוצאות של פונקציות.

## איך עושים את זה:
ב-Rust להדפסת דיבאג נעשה שימוש במאקרו `println!` או `eprintln!` להדפסה לפלט השגיאה.
```Rust
fn main() {
    let var = vec![1, 2, 3];
    println!("Debug: {:?}", var);
}
```
פלט לדוגמא:
```
Debug: [1, 2, 3]
```

כדי להשיג פלט יותר מורכב, נשתמש ב-`{:#?}` במקום ב-`{:?}`.
```Rust
fn main() {
    let var = vec![1, 2, 3];
    println!("Debug pretty-print: {:#?}", var);
}
```
פלט לדוגמא:
```
Debug pretty-print:
[
    1,
    2,
    3,
]
```

## הבט העמוק יותר
בעבר, לפני שפות כמו Rust קיימות, מתכנתים השתמשו ברשומות הדפסה או כתיבה לקובץ כדי לקבל משוב מן התוכנית. ב-Rust, הסטנדרט הוא להשתמש במאקרוס של הדפסה לפלט שבהם אפשר להדפיס ערכים או מבני נתונים שהם חלק מפיתרון הבעיה.

התכונה `Debug` דורשת שהטיפוס ייישם או יגדיר את תוכנית ההדפסה שלו. אלטרנטיבת הדפסה היא השימוש במאקרו `log!` בשילוב עם ספריות רישום (logging libraries) שמאפשרות לקבוע רמות דיבאג שונות.

## ראה גם
* מדריך התחלתי ל-Rust: https://www.rust-lang.org/learn/get-started
* מדריך למאקרוס `println!` ו`format!`: https://doc.rust-lang.org/std/fmt/
* ספריית רישום log: https://crates.io/crates/log
* ספריית env_logger (שילוב נפוץ עם log): https://crates.io/crates/env_logger
