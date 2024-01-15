---
title:                "בדיקת קיום תיקייה במחשב"
html_title:           "Rust: בדיקת קיום תיקייה במחשב"
simple_title:         "בדיקת קיום תיקייה במחשב"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## למה

למה אנשים מעוניינים בבדיקה אם ספרייה קיימת? עשה זאת כדי לוודא שהספרייה שאנו מנסים להשתמש בה קיימת במחשב שלנו ולאיזו מטרה היא משמשת.

## איך לעשות זאת

ברגע שנצרף למשתנה את שם הספרייה שאנו מעוניינים לבדוק האם היא קיימת במחשב שלנו, נוכל להשתמש בפונקציית "std::fs::metadata" של שפת ראסט כדי לבדוק את הנתונים הסטטיסטיים של הספרייה. להלן דוגמאות של קוד ופלט לבדיקת ספרייה קיימת ולא קיימת:

```Rust
// דוגמא לבדיקת ספרייה קיימת
use std::fs; 

fn main() {
    let directory = "/Users/username/Documents/Photos";
    let metadata = fs::metadata(directory).unwrap();
    println!("The directory {} exists.", directory);
}

// פלט:
// The directory /Users/username/Documents/Photos exists.


// דוגמא לבדיקת ספרייה לא קיימת
use std::fs; 

fn main() {
    let directory = "/Users/username/Documents/Music";
    let metadata = fs::metadata(directory);
    if metadata.is_ok() {
        println!("The directory {} exists.", directory);
    } else {
        println!("The directory {} does not exist.", directory);
    }
}

// פלט:
// The directory /Users/username/Documents/Music does not exist.
```

## הכנסה עמוקה

ברגע שאנו משתמשים בפונקציית "std::fs::metadata" לבדיקת ספרייה, אנו מקבלים נתונים סטטיסטיים נוספים כמו תאריך יצירת הספרייה, תאריך העדכון האחרון ורשימת ההרשאות של הספרייה. אם נרצה לבדוק רק את תאריך העדכון האחרון של הספרייה, נוכל להשתמש בפונקציית "std::fs::metadata" על אחת מהקבצים בספרייה ולקחת את הנתונים של התאריך מקומיים מהמידע שקיבלנו.

## ראה גם

- [תיעוד שפת ראסט על הפונקציה std::fs::metadata](https://doc.rust-lang.org/std/fs/fn.metadata.html)
- [אפיון שפת ראסט על השימוש בנתונים ס