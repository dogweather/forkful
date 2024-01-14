---
title:    "Rust: צוריקוב קובץ זמני"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## למה
יצירת קובץ זמני (temporary file) היא חלק חשוב מזהה התוכנה (identifier) בראסט (Rust). לא משנה אם אתם מפתחים משהו קטן או משהו גדול יותר, יצירת קבצים זמניים יכולה להיות בעצם כמו ליצור "תצורה" למידע חשוב במהלך קודד. כמו כן, זה גם יכול להיות שימושי כאשר אתם עובדים עם קבצים גדולים או עם רשת תקשורת.

## איך לעשות זאת
כדי ליצור קובץ זמני נשתמש בספריית פנימית של ראסט - `std::fs`. כאן תמצאו דוגמאות של כיצד ליצור קובץ זמני בקוד וכיצד לעבוד איתו.

```Rust
use std::fs::{self, File};
use std::io::{self, Write};
use std::path::Path;

// יצירת קובץ זמני בשם "temp_file.txt"
let tempdir = tempdir::TempDir::new("temp").expect("could not create temp directory");
let temp_file = tempdir.path().join("temp_file.txt");
    
// כתיבה לקובץ זמני
let mut file = File::create(&temp_file).expect("could not create temp file");
file.write_all(b"Hello, world!").expect("could not write to temp file");

// קריאה מקובץ זמני
let contents = fs::read_to_string(&temp_file).expect("could not read temp file");
println!("File contents: {}", contents); // Output: "Hello, world!"

// מחיקת הקובץ זמני
fs::remove_file(temp_file).expect("could not delete temp file");
```

## חצי נפילה
יצירת קובץ זמני משתמשת בקונספטים של "מאפיינים" (attributes) ו"קובץ אתחולי" (initialization files). המאפיינים מאפשרים לנו להחזיר את ההתמודדות הטובה גם כשמדובר על מצבי שימוש שונים כמו קודים תלת-ממד (multithreaded code), מחרוזות וכנפיק קודי. ניתן למצוא מידע נוסף על זה ב[תיעוד של ספריית `std::fs`](https://doc.rust-lang.org/std/fs/).

## ראו גם
* [מדריך לקבצים זמניים בשפת ראסט](https://rust-lang.github.io/mdBook/koans/paths/paths03.html)
* [הורה להעוף ממ