---
title:                "כתיבת בדיקות"
aliases:
- /he/rust/writing-tests/
date:                  2024-02-03T19:32:25.837430-07:00
model:                 gpt-4-0125-preview
simple_title:         "כתיבת בדיקות"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

כתיבת בדיקות בRust כוללת יצירת בדיקות אוטומטיות כדי לוודא שהקוד שלך מתנהג כצפוי. תכנתים עושים זאת כדי לתפוס באגים בשלב מוקדם, להקל על שינויי מבנה, ולשמור על איכות הקוד לאורך זמן.

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
