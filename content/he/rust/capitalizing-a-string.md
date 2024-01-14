---
title:                "Rust: להגדיר טקסט באותיות רישיות"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## למה

למה להשתמש בשפת תכנות ראסט לכיוון שלימיסוובנו משמש לכתיבת תוכניות אבטחון והאתגר הוא ליצור מתאם באמצעות ראסט כדי להשתמש בתכנות אחר.

## איך לעשות זאת

הנה דוגמא לתכנית פשוטה בראסט שמייצרת פלט של מחרוזת הכיוונית

```Rust
fn capitalize(string: &str) -> String {
    let mut chars = string.chars();
    if let Some(first_char) = chars.next() {
        first_char.to_uppercase()
            .chain(chars)
            .collect()
    } else {
        String::new()
    }
}
fn main() {
    let input = "hello";
    let output = capitalize(input);
    println!("{}", output);
}

```

פלט: "Hello"

## העומק של הנושא

כעת נכנסים לעומק על מנת להבין איך פונקציית "capitalize" עובדת. בשורה הראשונה, אנו יוצרים מופע חדש של "chars" שמכיל את מחרוזת הקלט. לאחר מכן, אנו בודקים אם יש תו ראשון במחרוזת, ואם כן, אנו משתמשים בפונקציה "to_uppercase" כדי להמיר את התו הראשון לאות גדולה. לאחר מכן, אנו משתמשים בפונקציה "chain" כדי לחבר את התו הראשון לתווים נוספים במחרוזת ובסופו של יום, אנו מחברים את כל התווים למחרוזת חדשה באמצעות פונקציית "collect". אם אין תווים במחרוזת, אנו משתמשים בפונקציית "String::new()" כדי ליצור מחרוזת ריקה.

## ראה גם

כאן מצורף רשימת קישורים שיכולים לעזור בהמשך לימוד ראסט:

- למד את ראסט באתר הרשמי שלה: https://www.rust-lang.org/he/learn
- קנו את הספר "ראסט במבוא" כדי למיין את כל הידע הנחוץ כדי להתחיל לפתח בראסט ביעילות: https://www.rust-lang.org/he/book
- אתר "Crates.io" מכיל מאגר רחב של חבילות ראסט בחינם: https://crates.io/