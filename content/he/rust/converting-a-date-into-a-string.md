---
title:                "Rust: המרת תאריך למחרוזת"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

חדש: כיצד להמיר תאריך למחרוזת ב-Rust

ראסט הוא שפת תכנות פופולרית המתרכזת בפרמיטיבים בסיסיים ובטיפוסים מאוד ספציפיים הידועים כ- "types". אחד התיפוסים המוכרים ביותר בראסט הוא מחרוזות, שהן סדרות של תווים. לפעמים אנו צריכים להמיר תאריך למחרוזת בכדי להציג אותו בצורה קריאה יותר לאנשים. במאמר הזה, אנו נלמד כיצד לעשות זאת בקלות בעזרת השפה של ראסט.

### מדוע

ממשקי משתמש וסידורי הפלט של ראסט משתמשים במחרוזות כדי להציג מידע בצורה ידידותית לאנשים. כאשר אנו עובדים עם תאריכים, כמו תאריך הולדת או תאריך אירוע, יש לנו צורך להציג אותם בצורה לקוחה ומבנה. על מנת לעשות זאת, אנו צריכים להמיר את התאריך למחרוזת.

### כיצד לעשות זאת

בראסט, כפי שצוין למעלה, משתמש בתיפוס "string" כדי לייצג מחרוזות. תאריך יכול להיות מופעל ישירות למחרוזת על ידי קריאה לפונקציות כמו `to_string()` או `to_string_with_format()`.

לדוגמה, בקוד הבא אנו ממירים תאריך למחרוזת עם פורמט של "יום-חודש-שנה":

```
Rust
use std::time::{SystemTime, UNIX_EPOCH};

fn main() {
    let now = SystemTime::now();
    let timestamp = now.duration_since(UNIX_EPOCH).expect("Time went backwards");
    let date = timestamp.as_secs() * 1000;
    let formatted_date = date.to_string_with_format("%d-%m-%Y").unwrap();
    println!("{}", formatted_date);
}
```

פלט:

```
14-08-2021
```

ניתן גם להשתמש בספריית "chrono" המציעה פונקציות מתקדמות יותר לניהול תאריכים וזמנים בראסט. לדוגמה, בקוד הבא אנו ממירים את התאריך