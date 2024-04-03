---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:41.364533-07:00
description: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E7\u05D5\u05D1\u05E5 \u05D8\
  \u05E7\u05E1\u05D8 \u05D1-Rust \u05DB\u05D5\u05DC\u05DC\u05EA \u05D9\u05E6\u05D9\
  \u05E8\u05D4, \u05DB\u05EA\u05D9\u05D1\u05D4 \u05D0\u05DC\u05D9\u05D5, \u05D5\u05DC\
  \u05E2\u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\u05D1\u05D5\u05EA, \u05D4\u05D5\
  \u05E1\u05E4\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05DC\u05E7\u05D5\u05D1\
  \u05E5 \u05E2\u05DC \u05DE\u05E2\u05E8\u05DB\u05EA \u05D4\u05E7\u05D1\u05E6\u05D9\
  \u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05D1\u05E6\u05E2\u05D9\
  \u05DD \u05E4\u05E2\u05D5\u05DC\u05D4 \u05D6\u05D5 \u05DB\u05D3\u05D9 \u05DC\u05E9\
  \u05DE\u05D5\u05E8 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD, \u05DB\u05DE\u05D5 \u05D9\
  \u05D5\u05DE\u05E0\u05D9\u2026"
lastmod: '2024-03-13T22:44:39.017988-06:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\
  \u05E1\u05D8 \u05D1-Rust \u05DB\u05D5\u05DC\u05DC\u05EA \u05D9\u05E6\u05D9\u05E8\
  \u05D4, \u05DB\u05EA\u05D9\u05D1\u05D4 \u05D0\u05DC\u05D9\u05D5, \u05D5\u05DC\u05E2\
  \u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\u05D1\u05D5\u05EA, \u05D4\u05D5\u05E1\
  \u05E4\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05DC\u05E7\u05D5\u05D1\u05E5\
  \ \u05E2\u05DC \u05DE\u05E2\u05E8\u05DB\u05EA \u05D4\u05E7\u05D1\u05E6\u05D9\u05DD\
  ."
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
weight: 24
---

## מה ולמה?
כתיבה לקובץ טקסט ב-Rust כוללת יצירה, כתיבה אליו, ולעיתים קרובות, הוספת נתונים לקובץ על מערכת הקבצים. מתכנתים מבצעים פעולה זו כדי לשמור נתונים, כמו יומני יישום, הגדרות, או תוכן שנוצר על ידי המשתמש, הבטיחות של הנתונים מעבר לתחום הביצוע של התוכנית.

## איך לעשות זאת:
ספריית הסטנדרט של Rust מספקת כלים חזקים לניהול קבצים, הממוקמים בעיקר בתוך המודולים `std::fs` ו-`std::io`. הנה דוגמה בסיסית ליצירת קובץ טקסט וכתיבה אליו:

```rust
use std::fs::File;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let mut file = File::create("hello.txt")?;
    file.write_all(b"Hello, world!")?;
    Ok(())
}
```

לאחר הפעלת הקוד הזה, תמצאו קובץ בשם `hello.txt` עם התוכן "Hello, world!".

לתרחישים מורכבים יותר, כמו הוספת טקסט לקובץ קיים או טיפול בנתונים גדולים ביעילות, Rust מציעה פונקציונליות נוספת. הנה איך להוסיף טקסט לקובץ קיים:

```rust
use std::fs::OpenOptions;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let mut file = OpenOptions::new()
        .write(true)
        .append(true)
        .open("hello.txt")?;
        
    file.write_all(b" Adding more text.")?;
    Ok(())
}
```

ההפעלה של זה תוסיף " Adding more text." לסוף הקובץ `hello.txt`.

במקרים מסוימים, שימוש בספריות צד שלישי יכול לפשט פעולות עבודה עם קבצים. הספרייה `serde`, בשילוב עם `serde_json`, לדוגמה, מאפשרת סידור וביטול סידור של מבני נתונים לתוך פורמט JSON וממנו, ומציעה גישה ברמה גבוהה לכתיבת קבצים:

```rust
use serde::{Serialize, Deserialize};
use serde_json;
use std::fs::File;

#[derive(Serialize, Deserialize)]
struct User {
    id: u32,
    name: String,
}

fn main() -> std::io::Result<()> {
    let user = User { id: 1, name: "Jane Doe".into() };
    let file = File::create("user.json")?;
    serde_json::to_writer(file, &user)?;
    Ok(())
}
```

לאחר הרצת הקוד הנ"ל, `user.json` יכיל נציגות בפורמט JSON של המבנה `User`. שימו לב ששימוש ב-`serde` ו-`serde_json` דורש הוספת הספריות האלה ל-`Cargo.toml` שלכם.

כתיבת קבצי טקסט ב-Rust, בין אם דרך ספריית הסטנדרט או בעזרת ספריות חיצוניות, היא דרך ישירה ועוצמתית לנהל את עמידות הנתונים ביישומים שלכם.
