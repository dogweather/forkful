---
date: 2024-01-26 04:36:06.786893-07:00
description: "XML, \u05E9\u05D4\u05D5\u05D0 \u05E8\u05D0\u05E9\u05D9 \u05EA\u05D9\u05D1\
  \u05D5\u05EA \u05E9\u05DC eXtensible Markup Language, \u05D4\u05D5\u05D0 \u05DB\u05DE\
  \u05D5 \u05D4\u05D1\u05DF \u05D3\u05D5\u05D3 \u05D4\u05DE\u05E1\u05D5\u05E8\u05D1\
  \u05DC \u05E9\u05DC JSON. \u05EA\u05EA\u05DE\u05D5\u05D3\u05D3 \u05E2\u05DD XML\
  \ \u05DB\u05D0\u05E9\u05E8 \u05D0\u05EA\u05D4 \u05DE\u05EA\u05E2\u05E1\u05E7 \u05E2\
  \u05DD \u05DE\u05E2\u05E8\u05DB\u05D5\u05EA \u05D9\u05E9\u05E0\u05D5\u05EA, \u05EA\
  \u05D5\u05DB\u05E0\u05D5\u05EA \u05D0\u05E8\u05D2\u05D5\u05E0\u05D9\u05D5\u05EA\
  \ \u05D0\u05D5 API-\u2026"
lastmod: '2024-02-25T18:49:37.266346-07:00'
model: gpt-4-0125-preview
summary: "XML, \u05E9\u05D4\u05D5\u05D0 \u05E8\u05D0\u05E9\u05D9 \u05EA\u05D9\u05D1\
  \u05D5\u05EA \u05E9\u05DC eXtensible Markup Language, \u05D4\u05D5\u05D0 \u05DB\u05DE\
  \u05D5 \u05D4\u05D1\u05DF \u05D3\u05D5\u05D3 \u05D4\u05DE\u05E1\u05D5\u05E8\u05D1\
  \u05DC \u05E9\u05DC JSON. \u05EA\u05EA\u05DE\u05D5\u05D3\u05D3 \u05E2\u05DD XML\
  \ \u05DB\u05D0\u05E9\u05E8 \u05D0\u05EA\u05D4 \u05DE\u05EA\u05E2\u05E1\u05E7 \u05E2\
  \u05DD \u05DE\u05E2\u05E8\u05DB\u05D5\u05EA \u05D9\u05E9\u05E0\u05D5\u05EA, \u05EA\
  \u05D5\u05DB\u05E0\u05D5\u05EA \u05D0\u05E8\u05D2\u05D5\u05E0\u05D9\u05D5\u05EA\
  \ \u05D0\u05D5 API-\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML"
---

{{< edit_this_page >}}

## מה ולמה?
XML, שהוא ראשי תיבות של eXtensible Markup Language, הוא כמו הבן דוד המסורבל של JSON. תתמודד עם XML כאשר אתה מתעסק עם מערכות ישנות, תוכנות ארגוניות או API-ים שדילגו על גלגל האופנה של JSON. הוא חיוני להחלפת נתונים שם XML עומד על רגליו.

## איך ל:
ב-Rust, ניתן להתמודד עם XML באמצעות crates כמו `xml-rs`. התקן על ידי הוספת `xml-rs = "0.8"` לקובץ `Cargo.toml` שלך. כך מנתחים XML פשוט:

```rust
extern crate xml;

use xml::reader::{EventReader, XmlEvent};

fn main() {
    let xml_data = r#"<book category="fiction">
    <title>Rust in Action</title>
    <author>Tim McNamara</author>
    <year>2021</year>
</book>"#;

    let parser = EventReader::from_str(xml_data);
    for e in parser {
        match e {
            Ok(XmlEvent::StartElement { name, .. }) => {
                println!("התחלה: {}", name);
            }
            Ok(XmlEvent::Characters(data)) => {
                println!("טקסט: {}", data);
            }
            Ok(XmlEvent::EndElement { name }) => {
                println!("סוף: {}", name);
            }
            Err(e) => {
                println!("שגיאה: {}", e);
            }
            _ => {}
        }
    }
}
```

פלט:
```
התחלה: book
התחלה: title
טקסט: Rust in Action
סוף: title
התחלה: author
טקסט: Tim McNamara
סוף: author
התחלה: year
טקסט: 2021
סוף: year
סוף: book
```
הקוד הזה עושה קריאת זרם של XML, מתמודד עם אלמנטים של התחלה וסיום ונתוני טקסט, ורושם כל שלב.

## צלילה עמוקה:
XML הוא ותיק בשנות הטכנולוגיה, נוצר לעולם האינטרנט בשנות ה-90 המאוחרות. הוא מבוסס על קריאות (למחשבים ולאנשים כאחד) ונתונים המתארים את עצמם ברחבי.

אלטרנטיבות? בוודאי, JSON הוא הבחירה המודרנית ל-API-ים באינטרנט, קל יותר ופחות רועש. בינתיים, YAML הפך לפופולרי עבור הגדרות, עם תצורה נקייה. אבל XML לא הולך לשום מקום בזמן הקרוב – תשתיות אדירות נבנו על גביו.

מאחורי הקלעים, פרסור XML ב-Rust מבוסס על דפוסי איטרטורים, שומר על שימוש בזיכרון נמוך וביצועים חדים. תמצא crates כמו `serde-xml-rs` לחוויה שדומה יותר ל-serde – מתנה עבור אלו הרגילים לטפל ב-JSON.

## ראה גם:
למידע נוסף על Rust ו-XML:
- `serde-xml-rs` לתאימות עם serde של Rust: [https://github.com/RReverser/serde-xml-rs](https://github.com/RReverser/serde-xml-rs)
- התיעוד הרשמי של Rust (כי תמיד כדאי לחדש את הידע): [https://doc.rust-lang.org/stable/book/](https://doc.rust-lang.org/stable/book/)
