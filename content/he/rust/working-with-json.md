---
title:                "עבודה עם JSON"
html_title:           "Arduino: עבודה עם JSON"
simple_title:         "עבודה עם JSON"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/working-with-json.md"
---

{{< edit_this_page >}}

## מה ולמה?
עבודה עם JSON מתארת את התהליך של שליחה, קבלה, ועיבוד של נתונים בפורמט JSON. תכנתים משתמשים בזה כיוון ש-JSON נפוץ, קריא וגמיש לשימוש בהעברת נתונים, במיוחד באפליקציות ווב.

## איך לעשות:
התעסקות עם JSON ב-Rust דורשת את חבילת `serde_json`. הדוגמה הבאה מראה כיצד לעשות סריאליזציה ודיסריאליזציה של אובייקט.

```Rust
use serde::{Serialize, Deserialize};
use serde_json::{Result, Value};

#[derive(Serialize, Deserialize)]
struct Person {
    name: String,
    age: u8,
    phones: Vec<String>,
}

fn main() -> Result<()> {
    let data = r#"
        {
            "name": "John Doe",
            "age": 43,
            "phones": [
                "+44 1234567",
                "+44 2345678"
            ]
        }"#;

    // דיסריאליזציה של מחרוזת JSON לאובייקט `Person`
    let p: Person = serde_json::from_str(data)?;

    // סריאליזציה של אובייקט `Person` למחרוזת JSON
    let serialized = serde_json::to_string(&p)?;

    println!("Serialized = {}", serialized);
    println!("Deserialized = {:?}", p);

    Ok(())
}
```

פלט דוגמה:
```
Serialized = {"name":"John Doe","age":43,"phones":["+44 1234567","+44 2345678"]}
Deserialized = Person { name: "John Doe", age: 43, phones: vec!["+44 1234567", "+44 2345678"] }
```

## עיון עמוק
JSON (JavaScript Object Notation) נוצר כתחליף קל משקל ל-XML. תקני SERDE ב-Rust מספקים פונקציונליות רבת עוצמה לצורך סריאליזציה ודיסריאליזציה. תחרותיות ל-JSON ב-Rust כוללות פורמטים כמו TOML ו-YAML, אבל JSON נשאר מוביל במיוחד לצורך שילוב עם JavaScript ו-APIs באינטרנט.

## ראה גם
- דוקומנטציה של `serde_json`: https://docs.serde.rs/serde_json/
- תיעוד Rust לעבודה עם JSON: https://doc.rust-lang.org/book/ch20-00-final-project-a-web-server.html
- המדריך רשמי ל-SERDE: https://serde.rs/