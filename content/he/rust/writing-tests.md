---
title:    "Rust: כתיבת בדיקות"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## למה

למה לכתוב בדיקות בראסט? בדיקות מהוות חלק חשוב בתהליך הפיתוח ויכולות לעזור לאתר באופן יעיל שגיאות ולהבטיח שהתוכנית עובדת כצפוי.

## כיצד לבצע

```rust
fn main() {
    assert_eq!(5 + 5, 10);
    println!("Test passed!");
}
```

כאן ניתן לראות דוגמא של בדיקת שגיאה בסיסית בראסט. על מנת לבצע בדיקות, ניתן להשתמש בפונקציות כמו `assert_eq!` ולהעביר להן את התוצאה הצפויה ואת התוצאה המתקבלת. אם השניים לא שווים, התוכנית תכשל ותדפיס הודעת שגיאה. ניתן להשתמש גם בפונקציות יותר מתקדמות כמו `assert!(expression)` לבדיקת ביטויים במקרים מיוחדים.

## מעמיקים יותר

רסט מספקת כמה דרכים לבצע בדיקות. למשל, ניתן ליצור מודול בשם `tests` לתיקיית הקוד ולהכיל בו את כל הבדיקות. ניתן גם להשתמש בסימולטור `cargo` כדי להריץ את כל הבדיקות בפעם אחת. כמו כן, חשוב לכתוב בדיקות שלמות ככל האפשר על מנת לוודא שהתוכנית עובדת בצורה נכונה.

## ראית גם

- [מדריך לבדיקות בראסט](https://doc.rust-lang.org/book/ch11-01-writing-tests.html)
- [מדיניות לבדיקות בראסט](https://rust-lang.github.io/rfcs/1574-more-tests.html)
- [דוגמאות בדיקות מתקדמות בראסט](https://github.com/rust-lang/rust/tree/master/src/test/ui)