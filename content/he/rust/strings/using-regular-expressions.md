---
title:                "שימוש בביטויים רגולריים"
aliases:
- /he/rust/using-regular-expressions/
date:                  2024-02-03T19:18:56.081586-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש בביטויים רגולריים"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

ביטויים רגולריים, או regex, מאפשרים למפתחים לחפש, להתאים ולעבד מחרוזות עם שיטות מתקדמות של התאמת תבניות. ב-Rust, שימוש ב-regex עוזר לפרסר ולטפל יעילות בנתוני טקסט, מה שהופך משימות כמו אימות נתונים, חיפושים, והמרות טקסט ליותר זרימים וקלים לתחזוק.

## איך לעשות:

הספרייה `regex` ב-Rust היא המקום הראשון לפנות אליו לעבודה עם ביטויים רגולריים. לשימוש בה, תצטרך להוסיף אותה תחילה לקובץ `Cargo.toml` שלך:

```toml
[dependencies]
regex = "1"
```

לאחר מכן, תוכל להתחיל ליישם פונקציונליות של regex בקוד Rust שלך. הנה איך לבצע כמה פעולות נפוצות:

### התאמת תבנית למחרוזת

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"^\d{4}-\d{2}-\d{2}$").unwrap();
    let date = "2023-04-15";

    println!("האם הטקסט תואם את תבנית התאריך? {}", re.is_match(date));
    // פלט: האם הטקסט תואם את תבנית התאריך? true
}
```

### מציאה וגישה להתאמות

```rust
use regex::Regex;

fn main() {
    let text = "Rust 2023, C++ 2022, Python 2021";
    let re = Regex::new(r"\b(\w+)\s(\d{4})").unwrap();

    for cap in re.captures_iter(text) {
        println!("שפה: {}, שנה: {}", &cap[1], &cap[2]);
    }
    // פלט:
    // שפה: Rust, שנה: 2023
    // שפה: C++, שנה: 2022
    // שפה: Python, שנה: 2021
}
```

### החלפת טקסט

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\b(\w+)\s(\d{4})").unwrap();
    let text = "Rust 2023, C++ 2022, Python 2021";
    let replaced = re.replace_all(text, "$1 עודכן ב-$2");

    println!("הטקסט שעודכן: {}", replaced);
    // פלט: הטקסט שעודכן: Rust עודכן ב-2023, C++ עודכן ב-2022, Python עודכן ב-2021
}
```

### פיצול טקסט באמצעות Regex

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\W+").unwrap(); // פיצול לפי כל תו שאינו מילה
    let text = "Rust-C++-Python-Go";

    let fields: Vec<&str> = re.split(text).collect();

    for field in fields {
        println!("שפה: {}", field);
    }
    // פלט:
    // שפה: Rust
    // שפה: C++
    // שפה: Python
    // שפה: Go
}
```

הדוגמאות האלה מספקות מדריך בסיסי להתחלה עם ביטויים רגולריים ב-Rust. ככל שהצרכים שלך הופכים למתוחכמים יותר, הספרייה `regex` מציעה שפע של פונקציונליות לטפל במשימות מורכבות של התאמת תבניות ועיבוד טקסט.
