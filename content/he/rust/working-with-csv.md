---
title:                "Rust: עבודה עם CSV"
simple_title:         "עבודה עם CSV"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## למה

CSV הוא פורמט נפוץ וחשוב למידע מבני נתונים, ובעזרת Rust ניתן לעבוד איתו בצורה מהירה ויעילה. כתיבת קוד ב־Rust תאפשר לך להתמודד בקלות עם כמויות גדולות של נתונים ולעבוד באופן מקצועי עם קבצי CSV.

## איך לעבוד עם CSV ב־Rust

ברוב המקרים, טיפוס הנתונים המתאים לנתוני CSV הוא קובץ המכיל שורות ועמודות עם כותרות עמודות בשורה ראשונה. ניתן להשתמש בספריית סטנדרטית של Rust, `csv`, כדי לקרוא ולכתוב נתונים מקובץ CSV. לדוגמה, נגדיר קובץ CSV עם שם "employees.csv" ונשתמש בקוד הבא כדי להדפיס את נתוני הקובץ בצורה נכונה:

```Rust
// קריאת הספריה
use csv::Reader;

// קריאת קובץ CSV
let mut reader = Reader::from_path("./employees.csv").unwrap();

// הדפסת הנתונים
for result in reader.records() {
    let record = result.unwrap();
    println!("{:?}", record);
}
```

כתוצאה מהקוד הזה, נקבל כפלט:

```Rust
["Name", "Age", "Position"]
["John Smith", "35", "Manager"]
["Sarah Cohen", "28", "Developer"]
["David Levy", "40", "Accountant"]
```

## Deep Dive

כעת נעבור לעומק ונדבר על אפשרויות נוספות לעבוד עם CSV ב־Rust. ראשית וראשונה, כדי למנוע בעיות כאשר נכתוב לקובץ CSV ונתונים הם מספרים עם נקודה עשרונית, נוכל להשתמש בספרייה `csv-core` המאפשרת לנו להגדיר את התצורה של הנתונים שנשתמש בהם במקרה זה. כמו כן, ניתן להשתמש בתכניות "משופרות" כמו `serde` על מנת לטעון נתונים ישירות מקבצי CSV למבנה מתאים למשתנים בקוד שלנו.

## ראה גם

- [הדרך היעילה לעבוד