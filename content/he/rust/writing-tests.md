---
title:                "Rust: כתיבת בדיקות"
simple_title:         "כתיבת בדיקות"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/writing-tests.md"
---

{{< edit_this_page >}}

## למה

כתיבת בדיקות תוכנה היא חלק חשוב מתהליך כתיבת קוד בכל שפת תכנות. היא מאפשרת לנו לוודא שהקוד שכתבנו עובד כפי שצריך ושהוא לא מונע כשלים מיותרים במערכת הפעלה או בתוכניות אחרות. בדיקות הן גם כלי לאיתור באגים בקוד ולתיקוןם במהירות.

## כיצד לכתוב בדיקות

קוד ה-Rust הבא מדגים כיצד לכתוב בדיקה פשוטה עבור פונקציה קטנה. הבדיקה מוודאת שהפונקציה מחזירה את התוצאה הנכונה עבור הפרמטרים הנתונים.

```Rust
fn multiply(x: u32, y: u32) -> u32{
    x * y
}

#[cfg(test)]
mod tests {
    use crate::multiply;

    #[test] // נקבע שזוהי בדיקה
    fn test_multiply() {
        assert_eq!(multiply(2, 3), 6); // מוודאים שהתוצאה שווה ל- 6
    }
}
```

מראה התוצאה המצורפת מוכיח שהבדיקה עבדה כמו שצריך:

```
    Running target/debug/deps/test_example-efypvsktztc

running 1 test
test tests::test_multiply ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

כעת, נביט קצת יותר באופן שבו ניתן לכתוב בדיקות מורכבות ולהשתמש במגוון של הפעולות והכלים הקיימים בשפת Rust.

## חפירה מעמיקה

בדיקות תוכנה ניתנות לכתיבה בכמה דרכים שונות ב-Rust. ניתן להשתמש במערכת בדיקות מובנית של השפה, בתוסף חיצוני כמו "Testify" או אפילו ספריית הכתיבה שלנו כדי לכתוב בדיקות צמודות יותר לקוד המקור.

כדי לכתוב בדיקות באופן תקף ויעיל, חשוב לקחת בחשבון את התכנים הבאים:

- התיקונים המגוונים היכולים להיות לבעיות בקוד כתוצאה משינוי