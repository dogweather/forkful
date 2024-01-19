---
title:                "אינטרפולציה של מחרוזת"
html_title:           "Arduino: אינטרפולציה של מחרוזת"
simple_title:         "אינטרפולציה של מחרוזת"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/interpolating-a-string.md"
---

{{< edit_this_page >}}

## מה זה, ומדוע?
חילוף מחרוזת הוא התהליך שבו משנים ערך מובנה במחרוזת עם מחצית אחרת. מתכנתים אוהבים להשתמש בו כדי ליצור מחרוזות בצורה גמישה יותר, מאפשרת תנאים הרצה דינמיים.

## איך:

אפשר ליצור כאלו מחרוזות באמצעות `println!("{}", value)`.

```Rust
fn main() {
    let name = "המתכנת הראשי";
    println!("שלום, {}!", name);
}
```

שם התוצאה היא:

```
שלום, המתכנת הראשי!
```

אפשר גם להשתמש במקום כדי ליצור מחרוזת ולהדפיס אותה אחר כך:

```Rust
fn main() {
    let name = "המתכנת הראשי";
    let greeting = format!("שלום, {}!", name);
    println!("{}", greeting);
}
```

## צלילה עמוקה:

חילוף מחרוזת הוא מאפיין מכר של שפות תכנות אחרות, כמו Perl וב-PHP. ב-Rust, השיטה הזו מנוסחת באופן מיוחד שמקשה על שגיאות בזמן ריצה.

מקומות פרמטרים אחרים נתמכים גם ב-Rust, אבל `println!` ו- `format!` הם נוחים מאוד ומכוונים היטב למתחילים.

הפקודה `println!` משתמשת ב-macro המיוחד של Rust כדי ליצור מחרוזת בזמן קומפילציה. אם אתה רוצה לבחון את ה-DSL (שפת Domain-Specific שפה שיש לה שפה של שפה מסוימת) שממש הוא, תוכל לעיין ב- [מקור הקוד](https://github.com/rust-lang/rust/blob/master/library/std/src/fmt/macros.rs).

## ראה גם:

- [מדריך Rust על מחרוזות מעוצבות](https://doc.rust-lang.org/book/ch02-02-strings.html#storing-strings)
- [מסמך האפיון של Rust לחילוף מחרוזת](https://doc.rust-lang.org/std/fmt/)