---
date: 2024-01-20 17:55:40.170563-07:00
description: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\
  \u05E1\u05D8 \u05D1\u05E4\u05E9\u05D8\u05D5\u05EA \u05D4\u05D9\u05D0 \u05DC\u05D4\
  \u05D6\u05E8\u05D9\u05DD \u05D0\u05EA \u05D4\u05EA\u05D5\u05DB\u05DF \u05E9\u05DC\
  \ \u05E7\u05D5\u05D1\u05E5 \u05DE\u05D4\u05D3\u05D9\u05E1\u05E7 \u05DC\u05EA\u05D5\
  \u05DA \u05D4\u05D6\u05D9\u05DB\u05E8\u05D5\u05DF \u05E9\u05DC \u05D4\u05DE\u05D7\
  \u05E9\u05D1. \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\
  \u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05E2\u05D1\u05D3\
  \ \u05D0\u05EA \u05D4\u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\u05D3\u05D5\u05D2\
  \u05DE\u05D0, \u05DC\u05E7\u05E8\u05D5\u05D0 \u05E7\u05D5\u05D1\u05E5 \u05E7\u05D5\
  \u05E0\u05E4\u05D9\u05D2\u05D5\u05E8\u05E6\u05D9\u05D4,\u2026"
lastmod: '2024-03-13T22:44:39.016320-06:00'
model: gpt-4-1106-preview
summary: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8 \u05D1\u05E4\u05E9\u05D8\u05D5\u05EA \u05D4\u05D9\u05D0 \u05DC\u05D4\u05D6\
  \u05E8\u05D9\u05DD \u05D0\u05EA \u05D4\u05EA\u05D5\u05DB\u05DF \u05E9\u05DC \u05E7\
  \u05D5\u05D1\u05E5 \u05DE\u05D4\u05D3\u05D9\u05E1\u05E7 \u05DC\u05EA\u05D5\u05DA\
  \ \u05D4\u05D6\u05D9\u05DB\u05E8\u05D5\u05DF \u05E9\u05DC \u05D4\u05DE\u05D7\u05E9\
  \u05D1. \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05E2\u05D1\u05D3 \u05D0\
  \u05EA \u05D4\u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DC\u05D3\u05D5\u05D2\u05DE\
  \u05D0, \u05DC\u05E7\u05E8\u05D5\u05D0 \u05E7\u05D5\u05D1\u05E5 \u05E7\u05D5\u05E0\
  \u05E4\u05D9\u05D2\u05D5\u05E8\u05E6\u05D9\u05D4,\u2026"
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
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
