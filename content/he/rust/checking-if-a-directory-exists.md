---
title:                "בדיקה אם ספרייה קיימת"
html_title:           "Java: בדיקה אם ספרייה קיימת"
simple_title:         "בדיקה אם ספרייה קיימת"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?

בדיקה אם ספריה קיימת בחלל מערכת הפעלה היא פעולה שמאפשרת לנו לוודא שנתיב מסוים קיים והוא ספריה לפני שנעשה איתו שימוש. מתכנתים מבצעים את הפעולה הזו למניעת שגיאות ריצת יישום.

## איך לעשות:

הנה דוגמא של קוד בשפת Rust שבודק אם ספריה קיימת:

```Rust
use std::path::Path;
fn main() {
    let path = Path::new("/some/path");
    if path.exists() && path.is_dir() {
        println!("The directory exists");
    } else {
        println!("The directory does not exist");
    }
}
```
פלט לדוגמה זו יכול להיות:

```
The directory does not exist
```

## בהקשר הרחב

Rust מתנהגת כך באופן מסורתי, בדיוק כפי שאנו מצפים משפות תכנות מערכת. חלופה לשימוש בפונקציה `exists()` הנ"ל היא שימוש במודול 'Metadata', אך הוא מורכב יותר לשימוש.
ביצוע הבדיקה נעשה דרך מערכת ההפעלה, כאשר השיחה למדעי המחשב משוואה את השאלה: האם הנתיב הזה קיים והאם הוא מצביע על ספריה?

## ראה גם:

- [חיבור Rust רשמי על std::path::Path](https://doc.rust-lang.org/std/path/struct.Path.html)
- [חיבור StackOverflow על בדיקת נתיב](https://stackoverflow.com/questions/26958489/how-to-check-if-a-path-exists-in-rust)
- [דיסקוסיה בפורום של Rust](https://users.rust-lang.org/t/how-to-check-if-a-file-exists/4918)