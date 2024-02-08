---
title:                "עבודה עם JSON"
date:                  2024-02-03T19:24:34.653598-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

עבודה עם JSON (תיאור אובייקט של JavaScript) ב-Rust היא על פרסור נתוני JSON לתוך מבני נתונים של Rust וסידור מחדש של מבני נתונים של Rust בחזרה ל-JSON. מתכנתים עושים זאת כדי להתמש ב-APIs של אינטרנט, קבצי קונפיגורציה, או כל פורמט החלפת נתונים שבו משתמשים ב-JSON בשל פורמטו הקליל והקריא לאדם.

## איך לעשות:

כדי לעבוד עם JSON ב-Rust, משתמשים באופן נרחב ב-crate של `serde` יחד עם `serde_json` לצורך סידור ופרסור מחדש. ראשית, וודאו לכלול אותם בקובץ `Cargo.toml` שלכם:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
```

### דוגמה 1: פרסור מחדש של JSON למבנה של Rust

הגדרת מבנה של Rust ושימוש במקרו של derive ל`Deserialize` וגם ל`Serialize`:

```rust
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
struct User {
    id: u32,
    name: String,
    email: String,
}

fn main() {
    let json_data = r#"
        {
            "id": 1,
            "name": "Jane Doe",
            "email": "jane.doe@example.com"
        }
    "#;

    let user: User = serde_json::from_str(json_data).unwrap();

    println!("מזהה משתמש: {}", user.id);
    println!("שם משתמש: {}", user.name);
    println!("דואר אלקטרוני של משתמש: {}", user.email);
}
```

**פלט:**

```
מזהה משתמש: 1
שם משתמש: Jane Doe
דואר אלקטרוני של משתמש: jane.doe@example.com
```

### דוגמה 2: סידור מחדש של מבנה של Rust ל-JSON

בשימוש באותו מבנה של `User`:

```rust
let user = User {
    id: 1,
    name: "Jane Doe".to_string(),
    email: "jane.doe@example.com".to_string(),
};

let json_data = serde_json::to_string(&user).unwrap();

println!("{}", json_data);
```

**פלט:**

```json
{"id":1,"name":"Jane Doe","email":"jane.doe@example.com"}
```

דוגמאות אלו מדגימות את התזרים הבסיסי של פרסור מחדש של JSON לתוך מבנים של Rust וסידור מבנים של Rust בחזרה למחרוזות של JSON. Serde מספקת ערכת כלים עשירה לעבודה עם JSON, כולל טיפול בשדות אופציונליים, קינונים מורכבים וסוגים שאינם נתמכים ישירות על ידי JSON.
