---
title:                "כתיבת בדיקות"
date:                  2024-01-19
simple_title:         "כתיבת בדיקות"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? / מה ולמה?
רשמתם קוד בראסט ורוצים לוודא שהוא עובד כשורה? כותבים טסטים. זה מאפשר לכם לוודא שהפונקציות שלכם עובדות כצפוי ולזהות בעיות לפני שהלקוחות שלכם עושים זאת.

## How to: / איך לעשות:
ראסט מביא עמו מערכת טסטים חזקה שכבר מובנית בשפה. כדי לכתוב ולהריץ טסטים, השתמשו במאקרו `#[test]` והפקודה `cargo test`.

```Rust
#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }

    #[test]
    fn another_test() {
        // הטסט הזה יכשל
        assert!(false);
    }
}
```

אחרי הרצת `cargo test`, תקבלו תוצאות כמו:
```
running 2 tests
test tests::it_works ... ok
test tests::another_test ... FAILED

failures:

---- tests::another_test stdout ----
thread 'tests::another_test' panicked at 'assertion failed: false', src/lib.rs:10:9
note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace

test result: FAILED. 1 passed; 1 failed; 0 ignored; 0 measured; 0 filtered out
```

## Deep Dive / צלילה עמוקה:
המערכת של ראסט לכתיבת טסטים נבנתה על אידיאולוגיית ה-"Test-Driven Development" (TDD). האלטרנטיבות הן בדיקות ידניות או שימוש בכלים חיצוניים, אבל ראסט מציע פתרון פשוט ויעיל יותר במרבית המקרים. המאקרו `#[test]` פשוט מציין שפונקציה מסוימת היא טסט, ו`cargo test` אוטומטית מזהה ומריץ אותה.

## See Also / ראו גם:
- המדריך הרשמי לטסטים בראסט: https://doc.rust-lang.org/book/ch11-00-testing.html
- מדריך ל-TDD בראסט: https://github.com/lloydmeta/tdd-in-rust
- Rust By Example, Testing: https://doc.rust-lang.org/rust-by-example/testing.html
