---
date: 2024-01-26 03:37:51.902008-07:00
description: "\u05D0\u05D9\u05DA \u05DC: \u05D1\u05D5\u05D0\u05D5 \u05E0\u05E8\u05E4\
  \u05E7\u05D8\u05D5\u05E8 \u05E7\u05D8\u05E2 \u05E7\u05D5\u05D3 \u05E4\u05E9\u05D5\
  \u05D8 \u05D1-Rust \u05DB\u05D3\u05D9 \u05DC\u05D4\u05E4\u05D5\u05DA \u05D0\u05D5\
  \u05EA\u05D5 \u05DC\u05D9\u05D5\u05EA\u05E8 \u05D0\u05D9\u05D3\u05D9\u05D5\u05DE\
  \u05D8\u05D9 \u05D5\u05E0\u05EA\u05D5\u05DF \u05DC\u05EA\u05D7\u05D6\u05D5\u05E7\
  \u05D4. \u05E0\u05EA\u05D7\u05D9\u05DC \u05E2\u05DD \u05E4\u05D5\u05E0\u05E7\u05E6\
  \u05D9\u05D4 \u05E9\u05DE\u05D7\u05E9\u05D1\u05EA \u05D0\u05EA \u05E1\u05DB\u05D5\
  \u05DD \u05E9\u05DC \u05D5\u05E7\u05D8\u05D5\u05E8 \u05E9\u05DC \u05DE\u05E1\u05E4\
  \u05E8\u05D9\u05DD \u05E9\u05DC\u05DE\u05D9\u05DD."
lastmod: '2024-03-13T22:44:39.000776-06:00'
model: gpt-4-0125-preview
summary: "\u05D1\u05D5\u05D0\u05D5 \u05E0\u05E8\u05E4\u05E7\u05D8\u05D5\u05E8 \u05E7\
  \u05D8\u05E2 \u05E7\u05D5\u05D3 \u05E4\u05E9\u05D5\u05D8 \u05D1-Rust \u05DB\u05D3\
  \u05D9 \u05DC\u05D4\u05E4\u05D5\u05DA \u05D0\u05D5\u05EA\u05D5 \u05DC\u05D9\u05D5\
  \u05EA\u05E8 \u05D0\u05D9\u05D3\u05D9\u05D5\u05DE\u05D8\u05D9 \u05D5\u05E0\u05EA\
  \u05D5\u05DF \u05DC\u05EA\u05D7\u05D6\u05D5\u05E7\u05D4."
title: "\u05E8\u05E4\u05E7\u05D8\u05D5\u05E8\u05D9\u05E0\u05D2"
weight: 19
---

## איך ל:
בואו נרפקטור קטע קוד פשוט ב-Rust כדי להפוך אותו ליותר אידיומטי ונתון לתחזוקה. נתחיל עם פונקציה שמחשבת את סכום של וקטור של מספרים שלמים:

```rust
fn sum(vec: &Vec<i32>) -> i32 {
    let mut sum = 0;
    for i in vec {
        sum += i;
    }
    sum
}

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    println!("The sum is {}", sum(&numbers));
}
```

פלט:
```
The sum is 15
```

עכשיו, בואו נרפקטור את זה כדי להשתמש ב-Rust היותר אידיומטי על ידי נצילות של איטרטורים ושיטת ה`fold`:

```rust
fn sum(vec: &[i32]) -> i32 {
    vec.iter().fold(0, |acc, &x| acc + x)
}

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    println!("The sum is {}", sum(&numbers));
}
```

אין שינוי בפלט – זה עדיין `15` – אבל הגרסה המרופקטרת נקייה יותר ומשתמשת בחוזקות של Rust כמו השאלה ושיטות איטרטור.

## טבילה עמוקה
הריפקטורינג הוא שורשיו בקהילת ה-Smalltalk והתפרסם בעולם ה-Java על ידי הספר של מרטין פאולר "Refactoring: Improving the Design of Existing Code". העקרונות שלו הם אוניברסליים וחלים גם על Rust, שבו בטיחות ומקביליות הם עליונים. Rust מעודד כתיבת קוד חזק על ידי זיהוי בעיות בזמן קומפילציה, ולכן במהלך ריפקטורינג, המהדר של Rust משמש כרשת בטיחות.

חלופות לריפקטורינג ידני כוללות שימוש בכלים אוטומטיים, כמו 'rustfmt' לעיצוב קוד ו-'clippy' לlinting, שיכולים להציע דרכים יותר אידיומטיות של כתיבת קוד. עם זאת, ריפקטורינג עמוק לעיתים דורש הבנה מושכלת של עיצוב הקוד, שאלו הכלים לא יכולים לאוטמט באופן מלא.

ב-Rust, ריפקטורינג עשוי להתמקד בשיפור שימוש בסוגים, ניצול יעיל של פרקי זמן, הפחתת הקצאות לא נחוצות, או שימוש בדפוסי מקביליות כמו שימוש ב-`Arc<Mutex<T>>` כשצריך. זה גם נפוץ לעבור מ-`unwrap()` לטיפול בשגיאות יותר ביטויי עם `Result<T, E>`.

## ראה גם
להעמקה נוספת בנושא ריפקטורינג ב-Rust:

- The Rust Book: https://doc.rust-lang.org/book/
- Rust by Example: https://doc.rust-lang.org/rust-by-example/
- Clippy, כלי linting של Rust: https://github.com/rust-lang/rust-clippy
- "Refactoring: Improving the Design of Existing Code" מאת Martin Fowler: https://martinfowler.com/books/refactoring.html
