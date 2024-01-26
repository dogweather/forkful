---
title:                "עבודה עם קבצי CSV"
html_title:           "Arduino: עבודה עם קבצי CSV"
simple_title:         "עבודה עם קבצי CSV"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## מה ולמה?
עיבוד קבצי CSV (ערכים מופרדי פסיקים) נדרש כאשר מעוניינים לטפל במידע טבולרי, כגון נתונים מאקסל או ייצוא נתונים ממסדי נתונים. מתכנתים עובדים עם CSV כי זה פורמט פשוט ונפוץ לשיתוף מידע כאשר גמישות וקריאות נדרשות.

## איך לעשות:
קריאת CSV וכתיבה אליו בראסט צפויה להיראות כך:

```Rust
use csv;
use std::error::Error;
use std::io;
use std::process;

fn read_csv() -> Result<(), Box<dyn Error>> {
    // יצירת reader מ-stdin
    let mut rdr = csv::Reader::from_reader(io::stdin());
    
    // ניווט דרך הרשומות
    for result in rdr.records() {
        let record = result?;
        println!("{:?}", record);
    }
    Ok(())
}

fn write_csv() -> Result<(), Box<dyn Error>> {
    // יצירת writer ל-stdout
    let mut wtr = csv::Writer::from_writer(io::stdout());

    // כתיבת שורה בתוך CSV
    wtr.write_record(&["רשומה1", "רשומה2", "רשומה3"])?;
    wtr.flush()?;
    
    Ok(())
}

fn main() {
    if let Err(err) = read_csv() {
        println!("שגיאה בקריאת CSV: {}", err);
        process::exit(1);
    }
    
    if let Err(err) = write_csv() {
        println!("שגיאה בכתיבת CSV: {}", err);
        process::exit(1);
    }
}
```
בעת הרצה, התוכנית תדפיס את הנתונים מ-CSV ותכתוב רשומה חדשה.

## עיון מעמיק
בעבר, עובדים השתמשו בספריית CSV שקיימת בסטנדרט של ראסט. כיום יש ספריות חיצוניות כמו `csv`, שפותחו להקל על פעולות CSV. אפשרות נוספת היא לעבוד עם serde, ספריית סיריאליזציה בראסט, שתומכת גם בעבודה עם CSV. נושא הקוד המשולב עם טיפול בשגיאות מאפיין את העבודה בראסט עם CSV, ומבטיח שהתוכנית תישאר יציבה גם אם מידע חסר או פגום.
 
## ראו גם
- מדריך רשמי לספריית CSV בראסט: https://docs.rs/csv
- דוקומנטציה רשמית של Serde: https://serde.rs
- פרויקטים לדוגמה עם Rust: https://github.com/BurntSushi/rust-csv
