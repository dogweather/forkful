---
title:                "הורדת דף אינטרנט"
html_title:           "C++: הורדת דף אינטרנט"
simple_title:         "הורדת דף אינטרנט"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## מה זה & למה?

להוריד את דף האינטרנט זהו תהליך של שמירה של המידע מדף אינטרנט מתוך שרת אינטרנט למחשב מקומי שלך. תכניתאים עושים זאת כדי למנות את המידע, לבחינתו או לשימוש בחיפוש מידע באופן אופליין.
  
## כיצד:
  
הנה דוגמא של קוד Rust להורדת דף אינטרנט:

```Rust
use std::fs::File;
use std::io::prelude::*;
use std::error::Error;
use reqwest;

fn main() -> Result<(), Box<dyn Error>> {
    let content = reqwest::blocking::get("https://www.example.com")?.text()?;
  
    let mut file = File::create("webpage.html")?;
  
    file.write_all(content.as_bytes())?;
  
    Ok(())
}
```

פלט הדוגמא הזו יצור קובץ בשם 'webpage.html' בספרייה של הפרויקט שלך עם את כל הנתונים מ 'https://www.example.com'.

## התעמקות עמוקה:

1. הקשר ההיסטורי: בעבר, הורדת דפי אינטרנט הייתה דרך נפוצה לכך שהמשתמשים נהנים מתוכן אינטרנט באופן אופליין. כיום, זה נמשך בצורה ממוחשבת, כמו דוגמאות חיפוש מידע.


2. חלופות: ישנם שפות תכנות אחרות עם ספריות שמאפשרות לך להוריד דפי אינטרנט, אבל Rust מספק ביצועים מרשימים ואבטחה משופרת.


3. פרטי התממשקות: הפונקציה `reqwest::blocking::get()` מבצעת בקשת HTTP GET ל- URL שקיבלה, והפונקציה `text()` מחזירה את תוכן התגובה כמחרוזת.

## ראו גם:

1. [Reqwest - Rust](https://docs.rs/reqwest/0.11.3/reqwest/)
2. [Rust תיעוד רשמי](https://www.rust-lang.org/)
3. [Rust Tutorial](https://www.rust-lang.org/learn)