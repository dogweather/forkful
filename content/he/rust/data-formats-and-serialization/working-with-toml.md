---
date: 2024-01-26 04:26:36.290115-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: ."
lastmod: '2024-03-13T22:44:39.026049-06:00'
model: gpt-4-0125-preview
summary: .
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD TOML"
weight: 39
---

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
