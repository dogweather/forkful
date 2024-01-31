---
title:                "רפקטורינג"
date:                  2024-01-26T03:37:51.902008-07:00
model:                 gpt-4-0125-preview
simple_title:         "רפקטורינג"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/refactoring.md"
---

{{< edit_this_page >}}

## מה ולמה?

ריפקטורינג הוא התהליך של מבנה מחדש של קוד מחשב קיים - שינוי הפקטורינג - מבלי לשנות את התנהגותו החיצונית. מתכנתים עושים זאת על מנת לשפר אתריבוטים לא פונקציונאליים של התוכנה, כמו קריאות, פחות מורכבות, שיפור יכולת התחזוקה, ויצירת ארכיטקטורה פנימית או מודל אובייקט ביטויי יותר כדי לשפר את הספיקות.

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
