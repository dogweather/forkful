---
title:                "עבודה עם TOML"
aliases:
- he/rust/working-with-toml.md
date:                  2024-01-26T04:26:36.290115-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/working-with-toml.md"
---

{{< edit_this_page >}}

## מה ולמה?
TOML הוא שפת סידור נתונים קריאה לאדם, שנעשית בה שימוש לעיתים קרובות עבור קובצי הגדרות. מתכנתים משתמשים ב-TOML בגלל הפשטות והבהירות שלה, כאשר היא מתרגמת בקלות למפת גיבוב ב-Rust.

## איך לעשות:
```Rust
// 1. כלל את Crate של 'toml' ב-Cargo.toml שלך
// [dependencies]
// toml = "0.5"

// 2. בצע נתיקה (Deserialize) של TOML למבנה (struct) ב-Rust
use toml::Value;

fn main() {
    let toml_content = r#"
        [server]
        host = "localhost"
        port = 8080
    "#;

    let value = toml_content.parse::<Value>().unwrap();
    let host = value.get("server").unwrap().get("host").unwrap();
    let port = value.get("server").unwrap().get("port").unwrap();
    
    println!("השרת פועל על {}:{}", host, port);
    // פלט: השרת פועל על "localhost":8080
}
```

## צלילה עמוקה
TOML, שמכוון ל-"שפה מינימלית, ברורה ובלתי מעורפלת של טום", נוצר על ידי טום פרסטון-ורנר ב-2013. היא שואפת להיות קריאה יותר מ-JSON או YAML עבור קבצי הגדרות. עיצובה של TOML מתמקד בתחביר לא מעורפל, מינימליזם, ומיפוי קל לסוגי נתונים.

אלטרנטיבות ל-TOML כוללות את JSON, YAML, ו-XML, אך TOML מנצחת בתרחישים שבהם קריאות אנושית ועריכת קבצים על ידי אנשים שאינם מתכנתים היא קריטית. כאשר עובדים עם TOML ב-Rust, serde מספק בסיס חזק לסידור וניתוק (serialization and deserialization), באמצעות traits למיפוי TOML על מבני Rust באופן חסר מאמץ.

אתגר בעבודה עם TOML הוא הדיוק שלה בסוגים ומבנה. המתכנת חייב להגדיר מערכת טיפוסים מבוססת מבנה ב-Rust שמשקפת את הסכמה של נתוני TOML כדי לנצל באופן יעיל את TOML ב-Rust.

## ראה גם
- [תיעוד TOML](https://toml.io/en/)
- [Crate של serde_toml](https://docs.rs/serde_toml/)
- [ספר שפת התכנות Rust](https://doc.rust-lang.org/stable/book/)
- [מאגר GitHub של TOML](https://github.com/toml-lang/toml)
