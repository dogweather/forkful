---
title:                "בדיקה האם תיקייה קיימת"
date:                  2024-01-20T14:58:32.503457-07:00
html_title:           "Gleam: בדיקה האם תיקייה קיימת"
simple_title:         "בדיקה האם תיקייה קיימת"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?
בודקים שקיים תיקייה כדי למנוע שגיאות תוכנה ולדעת אם אפשר להמשיך בפעולות שתלויות בתיקייה הזו. זה חשוב במיוחד בפעולות כמו קריאה וכתיבה לקבצים.

## איך לעשות:
```Rust
use std::path::Path;

fn main() {
    let path_str = "/example/directory";
    let path = Path::new(path_str);

    if path.exists() {
        println!("The directory exists!");
    } else {
        println!("The directory does not exist.");
    }
}
```

תוצאה אפשרית:
```
The directory exists!
```
או
```
The directory does not exist.
```

## טבילה עמוקה
בעבר, התכנתים בשפות כמו C דרשו יותר מאמץ לבדוק קיום תיקיות עם פונקציות מסובכות כמו `stat`. ב-Rust, הספרייה הסטנדרטית מספקת דרך נוחה ובטוחה לבדוק את קיום התיקייה עם המתודה `exists` של מבנה `Path`. אפשרויות חלופיות כוללות שימוש בספריות חיצוניות או בטכניקות ידניות, אבל רוב המקרים לא מחייבים את זה. חשוב לזכור שכאשר אנו עובדים עם מערכת קבצים, יש תמיד אתגרי זמינות והרשאות שעלולים לשנות מה שקוד התוכנה רואה.

## ראה גם
- תיעוד של `std::path`: https://doc.rust-lang.org/std/path/
- מדריך למערכת הקבצים ב-Rust: https://doc.rust-lang.org/book/ch12-00-an-io-project.html
- פורום התמיכה של Rust לשאלות ותשובות: https://users.rust-lang.org/
