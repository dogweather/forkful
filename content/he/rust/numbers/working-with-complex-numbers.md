---
title:                "עבודה עם מספרים מרוכבים"
aliases:
- /he/rust/working-with-complex-numbers/
date:                  2024-01-26T04:46:14.459453-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם מספרים מרוכבים"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
מספרים מרוכבים מורכבים מחלק ממשי וחלק מדומה והם חיוניים במגוון תחומים כגון הנדסה, פיזיקה, וגרפיקת מחשב. מתכנתים משתמשים בהם כדי לפתור משוואות שמספרים ממשיים רגילים אינם יכולים להתמודד איתם.

## איך לעשות:
ב-Rust אין תמיכה מובנית במספרים מרוכבים, אך crates כמו `num-complex` נמצאים שם בשבילך. הנה איך להשתמש בו:

```rust
use num_complex::Complex;

fn main() {
    let a = Complex::new(2.0, 3.0); // 2 + 3i
    let b = Complex::new(1.0, -4.0); // 1 - 4i

    let sum = a + b;
    let product = a * b;

    println!("Sum: {}", sum); // סכום: 3 - 1i
    println!("Product: {}", product); // מכפלה: 14 - 5i
}
```
תצטרך להוסיף את `num_complex` ל-`Cargo.toml` שלך כדי שכל זה יתרחש.

## צלילה עמוקה
מספרים מרוכבים הומצאו במאה ה-16 אך באמת התפרצו רק במאה ה-18 כאשר מתמטיקאים כמו אוילר התחילו לשחק איתם.

בלעדי פעולות מרוכבות מובנות, שפות כמו Rust מסתמכות על ספריות של צד שלישי. `num-complex` היא כזאת crate והיא חלק מאוסף crates עם שם `num` שמטרתו לספק סוגים ותכונות מספריים עבור Rust.

שווה לציין שחלק מהשפות (כמו Python) כוללות תמיכה מובנית במספרים מרוכבים, בעוד שאחרות (כמו C++, עם ה-header `<complex>`) מספקות אותם כחלק מהספרייה הסטנדרטית. ב-Rust, ההחלטה לשמור על ספריית סטנדרט קטנה פירושה שלעיתים קרובות תפנה ל-crates שנוצרו על ידי הקהילה עבור פונקציונליות נוספת.

## ראה גם
- [ספר ה-Rust](https://doc.rust-lang.org/book/): ללמוד עוד על Rust ואיך לעבוד עם crates חיצוניים.
- [ויקיפדיה - מספר מרוכב](https://en.wikipedia.org/wiki/Complex_number): להבנה עמוקה יותר של מספרים מרוכבים.
