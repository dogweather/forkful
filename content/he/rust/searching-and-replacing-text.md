---
title:    "Rust: חיפוש והחלפת טקסטים."
keywords: ["Rust"]
---

{{< edit_this_page >}}

## למה

כאשר אנו עובדים עם קוד בשפת ראסט, נתקלים לעיתים קרובות בצורך לחלף טקסט בקוד שלנו. לדוגמה, אם אנו רוצים לשנות את שם המשתנה או להחליף כמה פסקאות בקוד. המעבדה והזמן שתוקצה בניסיון לחפש ולהחליף בקוד גדול ומסורבל הן מעוררות מאוד. לכן, חיפוש והחלפה של טקסט הוא כלי חיוני עבור הקודם שנשתמש בכדי לשפר ולסדר את הקוד שלנו בצורה יעילה ומהירה.

## איך לעשות זאת

לחיפוש והחלפת טקסט יש תפקיד חשוב בתהליך השיפור של הקוד שלנו. ראסט מציעה שתי מתודות פנימיות לחיפוש והחלפת טקסט: `replace()` ו-`replace_range()`. 

```Rust
let mut text = "Hello, world!".to_string();

// חיפוש והחלפה של מחרוזת מסוימת
let new_text = text.replace("world", "Rust");
println!("{}", new_text); // "Hello, Rust!"

// חיפוש והחלפה של פסקה מסוימת במחרוזת
let new_text = text.replace_range(7..12, "Rust");
println!("{}", new_text); // "Hello, Rust!"
```

בנוסף, ניתן להשתמש גם בספקים (patterns) של רגולריים (regex) כדי להרחיב את האפשרויות של חיפוש והחלפת טקסט בראסט. נבין את כל האפשרויות במקרים שונים באמצעות הקוד הבא:

```Rust
use regex::Regex;

let text = "The quick brown fox jumps over the lazy dog.";

// אימות אם מחרוזת מתחילה באות גדולה
let cap_regex = Regex::new(r"^[A-Z]").unwrap();
println!("Starts with capital? {}", cap_regex.is_match(text)); // true

// חיפוש והחלפה של מילה במחרוזת
let word_regex = Regex::new(r"fox").unwrap();
let new_text = word_regex.replace(text, "elephant");
println!("{}", new_text); // "The quick brown elephant jumps over the lazy dog."

// חיפוש והחלפת מספרים במחרוזת
let num_regex = Regex::new(r"\d+").unwrap();
let new_text = num_regex.replace_all(text, "123");
println!("{}", new_text); // "The quick brown fox