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

שלום חברים! היום אני אראה לכם איך להוריד אתרי אינטרנט בשפת ראסט - גרסת התכנות הנוכחית.

## מה ולמה?
הורדת דף אינטרנט היא תהליך שבו משתמשים תוכנת מחשב כדי לקבל קובץ HTML שמתאר את התוכן של הדף. תוכנית המחשב שתעשה את זה יכולה להיות מאוד שימושית כאשר אתם פותחים אתר אינטרנט ואתם רוצים לבדוק שהכל עובד כפי שצריך.

## כיצד לעשות:
```Rust
use std::io::Read;
use std::fs::File;
use std::error::Error;
use std::io::Write;
use reqwest;

// Specify URL to download
let url = "https://www.example.com";

// Use reqwest library to make a GET request
let response = reqwest::blocking::get(url).expect("Unable to make request.");

// Check if response was successful
if response.status().is_success() {
    // Open a new file to write the response to
    let mut file = File::create("example.html").expect("Unable to create file.");

    // Read response and write to file
    response.copy_to(&mut file).expect("Unable to write to file.");
} else {
    println!("Request unsuccessful.");
}
```

## טיפול עמוק:
היסטוריית ההורדה של אתרי אינטרנט החלה עם פרוטוקולי HTTP ו-FTP המאפשרים הורדה מרוחקת של קבצים. כיום, יש גם כלים נוספים כמו ספריות של קיימות בשפות תכנות אחרות כמו Python ו-Java.

## ראו גם:
למידע נוסף על הורדת אתרי אינטרנט בראסט, ניתן לעיין במסמכי המדריכים בקוד הפתוח ובאתר הרשמי של ראסט.