---
title:    "Rust: חיפוש והחלפת טקסט"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מדוע
מחפשים ומחליפים טקסט היא פעולה חשובה בתוך תהליך התכנות וכתיבת קוד. היא מאפשרת לנו לשנות את הטקסט שלנו באופן מהיר ויעיל, ולהתאים את הקוד לצרכים שלנו. כאשר נעבוד עם שפת תכנות כמו ראסט (Rust), החיפוש וההחלפה של טקסט יכולה להיות כלי חשוב לכתיבת קוד יעיל וחדשני.

## כיצד לעשות זאת
עבור מטרת המדריך הזה, אנחנו נשתמש בספריית המובנית std::fs של ראסט כדי לקרוא ולבצע פעולות על קבצי טקסט. נתחיל עם פונקציות פשוטות שמאפשרות לנו לחפש ולהחליף טקסט בקבצים, ולאחר מכן נעבור לפונקציות מתקדמות יותר כדי להתאים את החיפוש וההחלפה לצרכים שלנו.

```Rust
use std::fs::File;
use std::io::{self, prelude::*, BufReader, BufWriter};

fn main() -> io::Result<()> {
    // קריאת הקובץ המקורי
    let input_file = File::open("input.txt")?;
    let reader = BufReader::new(input_file);

    // כתיבת הפלט המוחלף לקובץ חדש
    let output_file = File::create("output.txt")?;
    let mut writer = BufWriter::new(output_file);

    // חיפוש והחלפה של הטקסט
    for line in reader.lines() {
        // מבצע חיפוש והחלפה בכל טור של הקובץ
        let replaced_line = line?.replace("Rust", "תכנות");
        // כתיבת הטור המוחלף לקובץ החדש
        writer.write_all(replaced_line.as_bytes())?;
        writer.write_all(b"\n")?;
    }
    Ok(())
}
```

כמו שאתם רואים, אנו משתמשים בפונקציות כמו `read_to_string()` ו `replace()` של ספריית std::fs כדי לקרוא ולערוך טקסט בקבצים.

## Deep Dive
לחפש ולהחליף טקסט הוא עניין מעניין ומאתגר בתוך עולם תכנות. בנוס