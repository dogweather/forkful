---
title:                "עבודה עם json"
html_title:           "Rust: עבודה עם json"
simple_title:         "עבודה עם json"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/working-with-json.md"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם JSON הוא כתיבת קוד וקריאה לקובץ נתונים בפורמט מנויים. הפורמט נמצא בשימוש נרחב על ידי מפתחי תוכנה ומשמש כדי להעביר נתונים מבין שפות תכנות שונות.

## כיצד לעשות זאת:
הנה כמה דוגמאות לאיך לעבוד עם JSON בשפת Rust:

```
// לקיחת ערך מנתוני JSON:
let json = r#"{ "key": "value" }"#;
let data: Value = serde_json::from_str(json).unwrap();

// הוספת ערך לנתוני JSON:
let mut data = json!("key1": "value1");
data["key2"] = json!("value2");

// שינוי ערך בנתוני JSON:
data["key"] = Json::from("new value");

```

ברגע שנמצאים מחוץ לקובץ נתונים, אנחנו צריכים לעבוד עם נתוני JSON כמו רכיב חיצוני. Rust מציעה ספריות מובנות כמו serde_json לעבודה עם JSON בפשטות ויעילות.

## מעומק:
פורמט JSON נוצר כמענה לדרישה בכלי להעברת נתונים בין תכניות. הוא הפך לפופולרי כיום בזכות קריאותיות ופשטותו, המאפשרים למפתחי תוכנה לעבור בין שפות ומערכות שונות בקלות. אלטרנטיבות נפוצות לפורמט זה כוללות XML ו-YAML.

ספריות לעבודה עם JSON בשפת Rust כוללות גם serde_json, json, ו-json-rust. כל אחת מהן מציעה פיתוח נוח ויעיל לקוד שמתעסק עם נתוני JSON.

## ראו גם:
למידע נוסף על כתיבת קוד עם JSON ב-Rust, ניתן לבקר בדפי המידע הבאים:

- [המדריך הרשמי של Rust על ספריית serde_json](https://docs.serde.rs/serde_json/)
- [דף המידע הרשמי של ספריית json-rust](https://docs.rs/json-rust)
- [מדריך על כתיבת קוד JSON ב-Rust באתר Medium](https://medium.com/@Jotalpha/working-with-json-in-rust-f07f22bd7f87)