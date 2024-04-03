---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:25.837430-07:00
description: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA\
  \ \u05D1Rust \u05DB\u05D5\u05DC\u05DC\u05EA \u05D9\u05E6\u05D9\u05E8\u05EA \u05D1\
  \u05D3\u05D9\u05E7\u05D5\u05EA \u05D0\u05D5\u05D8\u05D5\u05DE\u05D8\u05D9\u05D5\u05EA\
  \ \u05DB\u05D3\u05D9 \u05DC\u05D5\u05D5\u05D3\u05D0 \u05E9\u05D4\u05E7\u05D5\u05D3\
  \ \u05E9\u05DC\u05DA \u05DE\u05EA\u05E0\u05D4\u05D2 \u05DB\u05E6\u05E4\u05D5\u05D9\
  . \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\
  \u05EA \u05DB\u05D3\u05D9 \u05DC\u05EA\u05E4\u05D5\u05E1 \u05D1\u05D0\u05D2\u05D9\
  \u05DD \u05D1\u05E9\u05DC\u05D1 \u05DE\u05D5\u05E7\u05D3\u05DD, \u05DC\u05D4\u05E7\
  \u05DC \u05E2\u05DC \u05E9\u05D9\u05E0\u05D5\u05D9\u05D9 \u05DE\u05D1\u05E0\u05D4\
  , \u05D5\u05DC\u05E9\u05DE\u05D5\u05E8\u2026"
lastmod: '2024-03-13T22:44:38.993520-06:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA \u05D1\
  Rust \u05DB\u05D5\u05DC\u05DC\u05EA \u05D9\u05E6\u05D9\u05E8\u05EA \u05D1\u05D3\u05D9\
  \u05E7\u05D5\u05EA \u05D0\u05D5\u05D8\u05D5\u05DE\u05D8\u05D9\u05D5\u05EA \u05DB\
  \u05D3\u05D9 \u05DC\u05D5\u05D5\u05D3\u05D0 \u05E9\u05D4\u05E7\u05D5\u05D3 \u05E9\
  \u05DC\u05DA \u05DE\u05EA\u05E0\u05D4\u05D2 \u05DB\u05E6\u05E4\u05D5\u05D9."
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA"
weight: 36
---

## איך לעשות:
מסגרת הבדיקות המובנית בRust תומכת בבדיקות יחידה, אינטגרציה, ותיעוד ללא הצורך בספריות חיצוניות. הבדיקות מסומנות באמצעות `#[test]`, וכל פונקציה שמסומנת כך מתורגמת כבדיקה.

### כתיבת בדיקת יחידה:
מקום בדיקות יחידה במודול שהן בודקות באמצעות תת-מודול `tests` שמסומן ב `#[cfg(test)]` כדי לוודא שהן מתורגמות רק בעת בדיקה.

```rust
// lib.rs או main.rs
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_adds_two() {
        assert_eq!(add(2, 2), 4);
    }
}
```

ביצוע הבדיקות:
```shell
$ cargo test
```

פלט:
```shell
   Compiling your_package_name v0.1.0 (/path/to/your_package)
    Finished test [unoptimized + debuginfo] target(s) in 0.00 secs
     Running unittests src/lib.rs (or src/main.rs)

running 1 test
test tests::it_adds_two ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

### כתיבת בדיקות אינטגרציה:
בדיקות אינטגרציה נמצאות בתיקיית tests ברמה העליונה של הפרויקט, לצד `src`. כל קובץ `.rs` ב`tests` מתורגם כקרייט נפרד שלו.

```rust
// tests/integration_test.rs
use your_package_name;

#[test]
fn it_adds_two() {
    assert_eq!(your_package_name::add(2, 2), 4);
}
```

### בדיקה עם ספריות שלישיות פופולריות:
לקבלת יכולת בדיקה רחבה יותר, ספריית ה`proptest` יכולה לייצר מגוון רחב של קלטים לבדיקת פונקציות.

הוסף `proptest` כתלות בפיתוח ב`Cargo.toml`:

```toml
[dev-dependencies]
proptest = "1.0"
```

השתמש ב`proptest` להרצת אותה הבדיקה עם קלטים רבים שנוצרים אוטומטית:

```rust
// בתוך tests/integration_test.rs או במודול's #[cfg(test)]

use proptest::prelude::*;

proptest! {
    #[test]
    fn doesnt_crash(a: i32, b:i32) {
        your_package_name::add(a, b);
    }
}
```

זה בודק ש `add` לא מתמוטט למגוון רחב של קלטים מסוג `i32`.
