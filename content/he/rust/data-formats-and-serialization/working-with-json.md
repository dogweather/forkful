---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:34.653598-07:00
description: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON (\u05EA\u05D9\u05D0\
  \u05D5\u05E8 \u05D0\u05D5\u05D1\u05D9\u05D9\u05E7\u05D8 \u05E9\u05DC JavaScript)\
  \ \u05D1-Rust \u05D4\u05D9\u05D0 \u05E2\u05DC \u05E4\u05E8\u05E1\u05D5\u05E8 \u05E0\
  \u05EA\u05D5\u05E0\u05D9 JSON \u05DC\u05EA\u05D5\u05DA \u05DE\u05D1\u05E0\u05D9\
  \ \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05E9\u05DC Rust \u05D5\u05E1\u05D9\u05D3\
  \u05D5\u05E8 \u05DE\u05D7\u05D3\u05E9 \u05E9\u05DC \u05DE\u05D1\u05E0\u05D9 \u05E0\
  \u05EA\u05D5\u05E0\u05D9\u05DD \u05E9\u05DC Rust \u05D1\u05D7\u05D6\u05E8\u05D4\
  \ \u05DC-JSON.\u2026"
lastmod: '2024-02-25T18:49:37.261609-07:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON (\u05EA\u05D9\u05D0\u05D5\
  \u05E8 \u05D0\u05D5\u05D1\u05D9\u05D9\u05E7\u05D8 \u05E9\u05DC JavaScript) \u05D1\
  -Rust \u05D4\u05D9\u05D0 \u05E2\u05DC \u05E4\u05E8\u05E1\u05D5\u05E8 \u05E0\u05EA\
  \u05D5\u05E0\u05D9 JSON \u05DC\u05EA\u05D5\u05DA \u05DE\u05D1\u05E0\u05D9 \u05E0\
  \u05EA\u05D5\u05E0\u05D9\u05DD \u05E9\u05DC Rust \u05D5\u05E1\u05D9\u05D3\u05D5\u05E8\
  \ \u05DE\u05D7\u05D3\u05E9 \u05E9\u05DC \u05DE\u05D1\u05E0\u05D9 \u05E0\u05EA\u05D5\
  \u05E0\u05D9\u05DD \u05E9\u05DC Rust \u05D1\u05D7\u05D6\u05E8\u05D4 \u05DC-JSON.\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD JSON"
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
