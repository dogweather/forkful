---
title:                "קריאת קובץ טקסט"
date:                  2024-01-20T17:55:40.170563-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת קובץ טקסט"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת קובץ טקסט בפשטות היא להזרים את התוכן של קובץ מהדיסק לתוך הזיכרון של המחשב. תוכניתנים עושים זאת כדי לעבד את הנתונים, לדוגמא, לקרוא קובץ קונפיגורציה, יומנים, או כאשר רוצים לשתף נתונים בין תהליכים.

## איך לעשות:
ב-Rust, קריאת קובץ טקסט היא עניין פשוט.

```Rust
use std::fs;

fn main() {
    let content = fs::read_to_string("path/to/your/file.txt")
        .expect("אופס, משהו השתבש בעת קריאת הקובץ");

    println!("תוכן הקובץ: {}", content);
}
```

אם כל הלך כשורה, התוכנית תדפיס את תוכן הקובץ למסך.

## צלילה לעומק
בעבר, קריאת קובץ טקסט הייתה תהליך מורכב יותר, עם שימוש בשיטות נמוכות יותר כמו עבודה ישירות עם זרמים ובפריסת המערכת. ראסט הופכת את זה לפשוט עם הפונקציה `read_to_string`, אבל יש גם אלטרנטיבות כמו `BufReader` לקריאה של קבצים גדולים באופן יעיל או עם טיפול בשגיאות ספציפיות. היכולת לקרוא קובץ עם ספריית ה-`std` של ראסט מספקת גישה לקריאה ועיבוד של טקסט ברוב השימושים הנפוצים.

## ראה גם
- [The Rust Programming Language - File I/O](https://doc.rust-lang.org/book/ch12-02-reading-a-file.html) - פרק מדריך השפה של ראסט על קלט/פלט לקבצים.
- [std::fs::read_to_string](https://doc.rust-lang.org/std/fs/fn.read_to_string.html) - התיעוד הרשמי לפונקציה שמשמשת לקריאת קבצי טקסט.
- [Rust by Example - Read Lines](https://doc.rust-lang.org/rust-by-example/std_misc/file/read_lines.html) - מדריך עם דוגמאות לקריאת שורות מקובץ.
