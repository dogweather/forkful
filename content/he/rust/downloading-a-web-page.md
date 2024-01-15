---
title:                "הורדת עמוד אינטרנט"
html_title:           "Rust: הורדת עמוד אינטרנט"
simple_title:         "הורדת עמוד אינטרנט"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## למה

למה צריך להוריד עמוד אינטרנט? המטרה העיקרית היא לקבל גישה לתוכן שמופיע באתרים שונים, כגון נתונים, תמונות וכתבות.

## כיצד לעשות זאת

הנה דוגמאות של קוד רוסט להורדת עמוד אינטרנט:

```rust
use std::io::prelude::*;
use std::fs::File;
use std::path::Path;
use reqwest::Client;

fn main() {
    // הגדרת כתובת האתר
    let url = "https://www.example.com";

    // יצירת קליינט ובקשת הגעה לעמוד האתר
    let client = Client::new();
    let mut response = client.get(url).send().unwrap();

    // יצירת קובץ חדש לשמירת התוכן
    let path = Path::new("example.html");
    let mut file = File::create(&path).unwrap();

    // קריאה של תוכן העמוד לקובץ ושמירתו
    let mut buffer = [0; 512];
    loop {
        let sz = response.read(&mut buffer).unwrap();
        if sz == 0 {
            break;
        }
        file.write_all(&buffer[..sz]).unwrap();
    }
}
```

פלט הקוד לאחר הרצתו הוא קובץ בשם "example.html" המכיל את כל התוכן שנמצא בכתובת האתר שצוינה בקוד. ניתן להשתמש בתוכן זה לצורך ניתוח, עיבוד או שימוש נוסף.

## מעמקים

להורדת עמוד אינטרנט באמצעות רוסט ניתן להשתמש בספריות שונות כגון reqwest או hyper. ספריות אלה מציעות פונקציות נוחות יותר כגון יצירת קליינטים או בקשות GET והן נכתבות בעבורת ויסטות פשוטה יותר.

תיעוד בספריות אלה גם מציע הרבה בוחן ופרטים נוספים שיכולים לסייע בהבנת תהליך ההורדה ובכתיבת קוד יעיל יותר.

## ראו גם

* [מדריך לנושא הפונקציונליות ברוסט](https://blog.khymos.org/2015/01/30/writing-clean-haskell-code-with-monads/)
* [תיעוד רשמי לספריית reqwest](https://docs.rs/reqwest/latest/reqwest/)
* [מדריך ל