---
title:                "שימוש בביטויים רגולריים"
date:                  2024-01-19
html_title:           "Bash: שימוש בביטויים רגולריים"
simple_title:         "שימוש בביטויים רגולריים"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## מה ולמה?
בעזרת ביטויים רגולריים, אנחנו מסננים ומעבדים טקסט בצורה מדויקת. תכניתנים משתמשים בהם כדי לחסוך זמן ולבצע משימות מורכבות בקלות.

## איך לעשות:
```Rust
use regex::Regex;

fn main() {
    let text = "מצא את המספרים: 123, 456, 789";
    let re = Regex::new(r"\d+").unwrap();
    
    for number in re.find_iter(text) {
        println!("מספר נמצא: {}", number.as_str());
    }
}
```

פלט:

```
מספר נמצא: 123
מספר נמצא: 456
מספר נמצא: 789
```

## צלילה עמוקה:
ביטויים רגולריים קיימים משנות ה-50 והשפיעו על שפות רבות. חלופות להם כוללות ניתוח סינטקטי מותאם או ביבליות ספציפיות לטיפול בטקסט. ב-Rust, הספרייה `regex` פותחה להיות מהירה ובטוחת זיכרון.

## ראו גם:
- [מדריך הביטויים הרגולריים של Rust](https://docs.rs/regex/)
- [מדריכים ומאמרים באתר rust-lang.org](https://www.rust-lang.org/learn)
- [פורום דיונים של Rust לשאלות ותמיכה](https://users.rust-lang.org/)
