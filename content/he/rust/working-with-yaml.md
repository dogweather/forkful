---
title:                "עבודה עם YAML"
date:                  2024-02-03T19:27:12.364647-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

בתכנות בשפת Rust, עבודה עם YAML (YAML Ain't Markup Language) מתייחסת לניתוח ויצירה של נתונים בפורמט YAML, תקן סידור נתונים ידידותי לאדם. מתכנתים משלבים טיפול ב-YAML ב-Rust כדי להגדיר אפליקציות, לנהל הגדרות או לעבד מבני נתונים מורכבים בפורמט ברור וקריא, נצלים את פשטותו לעומת JSON או XML לקבצי תצורה והחלפת נתונים.

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
