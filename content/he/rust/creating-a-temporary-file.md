---
title:                "יצירת קובץ זמני"
html_title:           "C#: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

---
## מה ולמה?
---
יצירת קובץ זמני היא פעולה שבה מכינים קובץ אשר מתנהל ומוחק אוטומטית על ידי המערכת. פרוגרמיסטים משתמשים בקבצים זמניים לשמור מידע לזמן קצר מאילו לעבודות כמו מיון פנימי ושמירתי ״סנאפשוטים״ זמניים. 

---
## איך לעשות:
---

Rust ספקת ביבליות Standard אשר כוללת את `std::fs::TempDir` ליצירת וטיפול בקבצים זמניים. 

```Rust
use std::fs::TempDir;

fn main() {
    let temp_dir = TempDir::new("my_temp_dir").unwrap();
    println!("Created temporary directory with path: {}", temp_dir.path().display());
}
```

בריצה, זה יייצר תיקייה זמנית, והפלט ייראה כך:

```Rust
Created temporary directory with path: /tmp/my_temp_dir.kLeXYo7L8fi1
```

----
## שיעור מעמיק:
---

הכנת קבצים זמניים היא פרקטיקה מסורתית בתחום התכנות. קבצים זמניים מספקים דרך קלה ומהירה לשמירה על הנתונים, עם הממשק של הקובץ המקומי, ובלי חיוב של נתונים באורך ארוך.

ב-Nix כמו מערכות, קבצים זמניים מיועדים להישאר רק עד לסיום התוכנית. מצד שני, בעיות עשו