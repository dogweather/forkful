---
title:                "עבודה עם YAML"
date:                  2024-01-19
html_title:           "Bash: עבודה עם YAML"
simple_title:         "עבודה עם YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? / מה ולמה?
YAML הוא פורמט תכנות שמשמש להגדרת הגדרות ותצורה. מתכנתים מטפלים ב-YAML כי הוא קריא לאנוש ומאוד נפוץ בתצורת פרויקטים ותשתיות.

## How to: / איך ל:
יש להתקין את החבילה `serde_yaml` לטיפול ב-YAML. הנה דוגמה של קוד פשוט לקריאה וכתיבת קובץ YAML:

```Rust
use serde::{Serialize, Deserialize};
use serde_yaml;
use std::fs;

#[derive(Debug, Serialize, Deserialize)]
struct Config {
    version: String,
    features: Vec<String>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let config_yaml = fs::read_to_string("config.yaml")?;
    let config: Config = serde_yaml::from_str(&config_yaml)?;
    
    println!("Read YAML: {:?}", config);
    
    let new_config = Config {
        version: "2.0".to_string(),
        features: vec!["feature1".to_string(), "feature2".to_string()],
    };
    let new_yaml = serde_yaml::to_string(&new_config)?;
    
    fs::write("new_config.yaml", new_yaml)?;
    
    Ok(())
}
```

כאשר הקובץ `config.yaml` ייראה כך:
```yaml
version: "1.0"
features:
  - "featureA"
  - "featureB"
```

## Deep Dive / נסיקה עמוקה:
YAML (YAML Ain't Markup Language) נולד ב-2001. מתחרה עיקרי הוא JSON, שנועד למכונה ולא לאדם. ל-YAML יתרון בקריאות אך עלול להתגלות פחות יעיל בזמן ריצה. כאשר עובדים עם YAML ב-Rust יש להיות מודעים לסכנות של הזרמת תוכן לא בטוח (unsafe content).

## See Also / גם כדאי לראות:
1. מדריך רשמי ל-YAML: https://yaml.org/spec/1.2/spec.html
2. דף הגיטהאב של Serde YAML: https://github.com/dtolnay/serde-yaml
3. מדריך לתכנות ב-Rust: https://doc.rust-lang.org/book/
4. פורום לתכנתים בעברית על Rust: https://rust-il.github.io/
