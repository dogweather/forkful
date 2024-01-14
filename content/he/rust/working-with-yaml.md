---
title:                "Rust: עובדים עם yaml"
simple_title:         "עובדים עם yaml"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

Hebrew translation:

# למה

הכתיבה בשפת ראסט היא חוויה מרתקת ומאתגרת, ואחד היתרונות שלה הוא היכולת לעבוד עם תבניות כמו YAML. השימוש בפורמט זה יכול להיות מועיל במיוחד כאשר אנו מנסים ליצור הגדרות מבוססות טקסט עבור אפליקציות שלנו. המאמר הזה יראה לכם איך ניתן לעבוד עם YAML בשפת ראסט.

# איך לעשות זאת

הראשון שאנו צריכים לעשות הוא להתקין את הספריות המתאימות על מנת לעבוד עם YAML. למרבה המזל, ישנן ספריות רבות זמינות לשימוש בראסט, כך שתהליך ההתקנה אינו יותר מכמה שורות קוד. כדי להתחיל, יש להוסיף את השורת הבאה לקובץ ה- `Cargo.toml` שלנו:

```Rust
yaml = "0.4"
```

לאחר ההתקנה, ניתן להתחיל לעבוד עם YAML בשפת ראסט. הכניסו את הרשימה שלכם או ההגדרה לתוך משנת טקסט עם מבנה YAML והשתמשו בספריית `serde_yaml` על מנת להמיר אותה למבנה נתונים שניתן לנהל ולעבד:

```Rust
use std::fs::File;
use serde_yaml::Value;

let file = File::open("my_file.yaml").expect("Unable to open file");
let data: Value = serde_yaml::from_reader(file).expect("Unable to parse YAML");

println!("{:#?}", data); 
```

תוצאת ההדפסה תהיה בפורמט הבא:

```Rust
Value(
    Mapping(
        {
            String(
                "user"
            ): Value(
                String(
                    "John"
                )
            ),
            String(
                "age"
            ): Value(
                Number(
                    26
                )
            ),
        }
    )
)
```

ניתן לעבור על הנתונים ולהשתמש בהם כרצוננו, ובנוסף ניתן להמיר חזרה למבנה YAML בעזרת פונקציות כמו `serde_yaml::to_string()`.

# גילוי נתונים מעמיק

אם אתם מעוניינים להתקדם וללמוד עוד על עבודה עם YAML בשפת ראסט, נ