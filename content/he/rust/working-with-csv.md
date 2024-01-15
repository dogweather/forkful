---
title:                "עובדים עם CSV"
html_title:           "Rust: עובדים עם CSV"
simple_title:         "עובדים עם CSV"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## למה 

ראשית ראשית, אנחנו רוצים לעבוד עם קבצי CSV כדי לאכתוב תכניות שמתמקדות בנתונים. קבצי CSV הם פורמט פשוט ונגיש שמאפשר לנו לשמור נתונים בצורה מכוונת ולהתמקד בעיבוד מידע.

## כיצד לא

תחילה, נצטרך להתקין את החבילה "csv" באמצעות פקודת "cargo install csv". כדי לעבוד עם קובץ CSV קיים, אנחנו נשתמש בפונקציית "from_reader". נמיר את הנתונים לסוג נתונים מתאים כדי להשתמש בהם כמו שנרצה. למשל, ניתן להדפיס את הנתונים בעזרת הפונקציה "println!" כדי לבדוק את הנתונים שנקראו.

```Rust
extern crate csv;

use std::error::Error;
use std::fs::File;
use std::process;

use csv::Reader;

fn main() -> Result<(), Box<dyn Error>> {
    let file = File::open("./data.csv")?;
    let mut reader = Reader::from_reader(file);
    for result in reader.records() {
        let record = result?;
        println!("{:?}", record);
    }
    Ok(())
}
```

פלט:

```Rust
["מנה", "מחיר", "תאריך"]
["פיצה", "50", "01/01/2020"]
["סנדוויץ׳", "25", "02/01/2020"]
["סלט", "20", "03/01/2020"]
```

## במקום עמוק

כעת שאנחנו מכירים את היסודות, נוכל לעזור לעצמנו עם פונקציות נוספות כמו "trim", "retain", ו"to_string". ניתן גם להשתמש במודולים נוספים כמו "serde" כדי לקרוא קבצים CSV מהיר ויעיל יותר.

## ראה גם

- [מדריך מלא לעבוד עם חבילת CSV ב-Rust](https://docs.rs/csv/1.0.0/csv/)
- [קוד מקור למימוש פונקציות נוספות עבור קבצי CSV](https://github.com/BurntSushi/rust-csv)
- [מדריך מכיר ב-Rust](https://www.rust-lang.org/learn)