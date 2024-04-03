---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:12.364647-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: Rust \u05D0\u05D9\
  \u05E0\u05D5 \u05EA\u05D5\u05DE\u05DA \u05D1-YAML \u05D1\u05E1\u05E4\u05E8\u05D9\
  \u05D9\u05EA \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\u05D8 \u05E9\u05DC\u05D5, \u05D5\
  \u05DC\u05DB\u05DF \u05D0\u05E0\u05D5 \u05D1\u05D3\u05E8\u05DA \u05DB\u05DC\u05DC\
  \ \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1-crates \u05E6\u05D3 \u05E9\u05DC\
  \u05D9\u05E9\u05D9 \u05DB\u05DE\u05D5 `serde` (\u05DC\u05E1\u05D9\u05D3\u05D5\u05E8\
  \ \u05D5\u05E0\u05D9\u05EA\u05D5\u05D7 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD) \u05D1\
  \u05E9\u05D9\u05DC\u05D5\u05D1 \u05E2\u05DD\u2026"
lastmod: '2024-03-13T22:44:39.021172-06:00'
model: gpt-4-0125-preview
summary: "Rust \u05D0\u05D9\u05E0\u05D5 \u05EA\u05D5\u05DE\u05DA \u05D1-YAML \u05D1\
  \u05E1\u05E4\u05E8\u05D9\u05D9\u05EA \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\
  \ \u05E9\u05DC\u05D5, \u05D5\u05DC\u05DB\u05DF \u05D0\u05E0\u05D5 \u05D1\u05D3\u05E8\
  \u05DA \u05DB\u05DC\u05DC \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1-crates\
  \ \u05E6\u05D3 \u05E9\u05DC\u05D9\u05E9\u05D9 \u05DB\u05DE\u05D5 `serde` (\u05DC\
  \u05E1\u05D9\u05D3\u05D5\u05E8 \u05D5\u05E0\u05D9\u05EA\u05D5\u05D7 \u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD) \u05D1\u05E9\u05D9\u05DC\u05D5\u05D1 \u05E2\u05DD `serde_yaml`."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD YAML"
weight: 41
---

## איך לעשות:
Rust אינו תומך ב-YAML בספריית הסטנדרט שלו, ולכן אנו בדרך כלל משתמשים ב-crates צד שלישי כמו `serde` (לסידור וניתוח נתונים) בשילוב עם `serde_yaml`.

ראשית, הוסף תלות לקובץ `Cargo.toml` שלך:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_yaml = "0.8"
```

עכשיו, בואו נראה איך לנתח מחרוזת YAML למבנה Rust ולסדר מחדש מבנה Rust למחרוזת YAML.

### ניתוח YAML למבני Rust
הגדר מבנה Rust שמשקף את הנתונים שאתה מצפה לקבל ב-YAML. השתמש בתכונות Serde להתאמה אישית אם נחוץ.

```rust
use serde::{Deserialize, Serialize};
use serde_yaml;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Config {
    name: String,
    durability: i32,
    owner: Owner,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Owner {
    name: String,
    age: i32,
}

fn main() {
    let yaml_data = "
name: Shield
durability: 300
owner:
  name: Steve
  age: 25
";

    let deserialized_config: Config = serde_yaml::from_str(yaml_data).unwrap();
    println!("{:?}", deserialized_config);
}
```

דוגמת פלט לאחר הרצת הקוד Rust לעיל תהיה:

```plaintext
Config { name: "Shield", durability: 300, owner: Owner { name: "Steve", age: 25 } }
```

### סידור מבני Rust ל-YAML
דוגמה זו לוקחת את המבנה `Config` מהסעיף הקודם וסודרת אותו בחזרה לפורמט YAML.

```rust
fn main() {
    let config = Config {
        name: String::from("Axe"),
        durability: 120,
        owner: Owner {
            name: String::from("Alex"),
            age: 30,
        },
    };

    let serialized_yaml = serde_yaml::to_string(&config).unwrap();
    println!("{}", serialized_yaml);
}
```

הפלט הצפוי יהיה מחרוזת מסודרת בפורמט YAML:

```yaml
---
name: Axe
durability: 120
owner:
  name: Alex
  age: 30
```

הקטעים האלו מדגימים כיצד לשלב ניתוח וייצור של YAML באפליקציות Rust שלכם ביעילות, באמצעות ה-crates הפופולריים `serde` ו-`serde_yaml`, מתאימים למבני נתונים מורכבים ומספקים תצורות פשוטות וקריאות לאדם.
