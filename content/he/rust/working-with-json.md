---
title:                "Rust: עובדים עם json"
simple_title:         "עובדים עם json"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/working-with-json.md"
---

{{< edit_this_page >}}

## למה

כאשר מתחילים ללמוד פיתוח בשפת ראסט, ייתכן שירצו להתחיל לעבוד עם JSON. כפי שיודעים, ישנם המון צורות שונות להתקשר עם נתונים ואחת מהן היא JSON. שפת ראסט מציעה כלים נהדרים לטיפול ועיבוד בנתונים מסוג זה ולכן כדאי ללמוד את הטכניקות הנכונות כדי להשתמש בהם בצורה יעילה.

## איך לעשות זאת

כדי לעבוד עם JSON בשפת ראסט, נצטרך להשתמש בספריית מתאימה. הספרייה הנפוצה והנמצאת בצמוד לשפת ראסט היא `serde_json`. בקצרה, ספריית זו מאפשרת לנו לקרוא ולכתוב נתונים בתבנית JSON. ניתן להתקין את הספרייה על ידי הרצת הפקודה `cargo install serde_json` בטרמינל.

כדי להשתמש בספרייה זו, נצטרך ליבנות עבורה טיפוסים מתאימים. לדוגמה, ניתן ליצור מבנה `User` המכיל שדות כגון `name` ו-`age`:

```rust
use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize)]
struct User {
    name: String,
    age: u32,
}
```

כעת, ניתן ליצור משתמש חדש ולהמיר אותו לתבנית JSON באמצעות פעולת `serde_json::to_string()`:

```rust
let user = User { 
    name: String::from("John"), 
    age: 25 
};

let json_string = serde_json::to_string(&user).unwrap();
println!("{}", json_string);
```

פלט של קוד זה יהיה:

```console
{"name":"John","age":25}
```

ניתן להמיר גם מחרוזת JSON למבנה מתאים באמצעות פעולת `serde_json::from_str()`:

```rust
let json_string = r#"{"name": "Jane", "age": 30}"#;
let user: User = serde_json::from_str(json_string).unwrap();
```

פלט של קוד זה יהיה ערך של משתנה `user` המכיל את השדות `name` ו-`age` בהתאמה.

כמובן, ישנם עוד המון פעולות ואפשרויות לעבוד עם JSON בש