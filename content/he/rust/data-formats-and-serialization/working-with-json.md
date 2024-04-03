---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:34.653598-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05DB\u05D3\u05D9\
  \ \u05DC\u05E2\u05D1\u05D5\u05D3 \u05E2\u05DD JSON \u05D1-Rust, \u05DE\u05E9\u05EA\
  \u05DE\u05E9\u05D9\u05DD \u05D1\u05D0\u05D5\u05E4\u05DF \u05E0\u05E8\u05D7\u05D1\
  \ \u05D1-crate \u05E9\u05DC `serde` \u05D9\u05D7\u05D3 \u05E2\u05DD `serde_json`\
  \ \u05DC\u05E6\u05D5\u05E8\u05DA \u05E1\u05D9\u05D3\u05D5\u05E8 \u05D5\u05E4\u05E8\
  \u05E1\u05D5\u05E8 \u05DE\u05D7\u05D3\u05E9. \u05E8\u05D0\u05E9\u05D9\u05EA, \u05D5\
  \u05D5\u05D3\u05D0\u05D5 \u05DC\u05DB\u05DC\u05D5\u05DC \u05D0\u05D5\u05EA\u05DD\
  \ \u05D1\u05E7\u05D5\u05D1\u05E5\u2026"
lastmod: '2024-03-13T22:44:39.022728-06:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05D3\u05D9 \u05DC\u05E2\u05D1\u05D5\u05D3 \u05E2\u05DD JSON \u05D1\
  -Rust, \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D0\u05D5\u05E4\u05DF\
  \ \u05E0\u05E8\u05D7\u05D1 \u05D1-crate \u05E9\u05DC `serde` \u05D9\u05D7\u05D3\
  \ \u05E2\u05DD `serde_json` \u05DC\u05E6\u05D5\u05E8\u05DA \u05E1\u05D9\u05D3\u05D5\
  \u05E8 \u05D5\u05E4\u05E8\u05E1\u05D5\u05E8 \u05DE\u05D7\u05D3\u05E9."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON"
weight: 38
---

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
