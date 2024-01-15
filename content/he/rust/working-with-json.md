---
title:                "עבודה עם JSON"
html_title:           "Rust: עבודה עם JSON"
simple_title:         "עבודה עם JSON"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/working-with-json.md"
---

{{< edit_this_page >}}

## Why
למה אתה בעל עניין לעבוד עם JSON בשפת ראסט?

ראסט היא שפת תכנות מודרנית ויעילה הנהוגה בעולם תוכנה. אחת היכולות החזקות שלה היא יכולת לעבוד בקלות עם פורמטים תקשורת מקבילים כמו JSON. שימוש בפורמט הזה נמצא בכל מקום בעולם התכנות כעת, ולכן היא מיועדת לספק לך כלי יעיל לעבוד עם נתוני JSON.

## How To

עכשיו שאתה מבין למה חשוב לעבוד עם JSON, בוא נלמד איך לעבוד עם זה בראסט.

נתחיל עם ייבוא המודול של JSON:

```Rust
// ייבוא המודול
use serde_json::{Value, json};
```

עכשיו, ניצור משתנה נתמך של סוג JSON:

```Rust
let json_data = r#"{
    "name": "John",
    "age": 30,
    "address": {
        "country": "USA",
        "city": "New York"
    },
    "hobbies": [
        "hiking",
        "reading",
        "cooking"
    ]
}"#;
```

עם עזרת פונקציית ייבוא מובנית מטיפוס שלנו לסוג JSON, נפעיל את הנתמך ונשלב אותו למשתנה נתמך:

```Rust
let parsed_json: Value = json::from_str(json_data).unwrap();
```

לבסוף, נוכל להשתמש בסוג JSON המתאים כדי לגשת לנתונים:

```Rust
// גישה לשדה name
let name = parsed_json["name"].as_str().unwrap();
// גישה לשדה country בתת סוג JSON של כתובת
let country = parsed_json["address"]["country"].as_str().unwrap();
// גישה לרשימת התחביבים
let hobbies = parsed_json["hobbies"].as_array().unwrap();
// גישה לתחביב מספר 2 ברשימה
let hobby2 = hobbies[1].as_str().unwrap();
```

לבסוף, נדפיס בתצוגה יפה את הנתונים שלנו:

```Rust
println!("Name: {}", name);
println!("Country: {}", country);
println!("Hobbies: {}", hobbies);
println!("Second Hobby: {}", hobby2);
```

הפלט יראה כך:

```
Name: John
Country: USA
Hobbies: ["hiking", "reading", "cooking"]
Second Hobby: reading
```

## Deep Dive

עם יכולות העיבוד של ספריית `serde_json`, ניתן לקבל נקודת יציאה מצוינת לט