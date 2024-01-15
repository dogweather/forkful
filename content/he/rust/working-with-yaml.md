---
title:                "עבודה עם yaml"
html_title:           "Rust: עבודה עם yaml"
simple_title:         "עבודה עם yaml"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## מדוע

אתם יכולים להשתמש ב־YAML כדי לארגן ולשמור נתונים בקוד שלכם. הוא מתאים במיוחד לשימוש במערכות גדולות ורבות, והוא פשוט וקל לשימוש.

## איך לעשות

הנה כמה דוגמאות שיכולות לעזור לכם להתחיל עם YAML בשפת ראסט:

```rust
// ייבאו את ספריית "serde_yaml"
extern crate serde_yaml;
use serde_yaml::Value;

// ייצוא נתונים לקובץ YAML
fn export_to_yaml(data: &Value) -> serde_yaml::Result<String> {
    // קבלו את הנתונים בתבנית YAML
    let yaml = serde_yaml::to_string(&data)?;
    
    // הוסיפו את הנתונים לקובץ חדש
    std::fs::write("data.yaml", &yaml)?;
    
    // תאפשרו למשתמש לדעת שהפעולה הושלמה בהצלחה
    Ok(String::from("נתונים יוצאים במוצרים!"))
}

// ייבאו נתונים מקובץ YAML
fn import_from_yaml() -> serde_yaml::Result<()> {
    // קראו את קובץ ה־YAML והפוך אותו לנתוני Value
    let data = &serde_yaml::from_str(std::fs::read_to_string("data.yaml")?.as_str())?;
    
    // תדפיסו את הנתונים שנמצאים בתוך ה־Value
    for (key, value) in data.as_mapping().unwrap() {
        println!("{}: {}", key.as_str().unwrap(), value.as_i64().unwrap());
    }
    
    Ok(())
}
```

אם תרצו לתרגם נתונים מפורמט אחר ל־YAML, תוכלו להשתמש בספריית "serde", שמאפשרת לכם לסדר את הנתונים בפורמט של YAML. לפרטים נוספים, ניתן לקרוא את המדריך המפורט [כאן](https://serde.rs/).

## חשיפה מעמיקה

כעת שאנחנו כבר מכירים את כמה סיבות לשימוש ב־YAML ואת האופן להעביר נתונים לקובץ YAML וממנו, ניתן לעיין בכמה נכונויות נוספות בשימוש בנתוני YAML עם ראסט.

במציאות, הרבה מפריטי התצורה נמצאים בפורמט YAML ואתם תמצאו שהעבודה עם נתונים בפור