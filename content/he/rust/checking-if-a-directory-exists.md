---
title:                "Rust: בדיקת קיום תיקייה"
simple_title:         "בדיקת קיום תיקייה"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## למה

האם אתם נתקלים במצבים שבהם אתם צריכים להבין אם תיקייה קיימת במקום מסוים במחשב שלכם? כמו במצבים שבהם אתם רוצים ליצור תיקייה חדשה אבל רוצים לוודא שהתיקייה לא קיימת כבר? במאמר הזה נלמד כיצד לבדוק אם תיקייה קיימת ב-Rust ונבין למה זה חשוב.

## כיצד לעשות זאת

בכדי לבדוק האם תיקייה קיימת ב-Rust, נצטרך להשתמש בפונקציה נקראת `Path::is_dir()` שמקבלת את הנתיב של התיקייה כפרמטר ומחזירה בערך בוליאני המציין האם התיקייה קיימת או לא. לפניכם תוצאה נכונה של קוד שהוסבר:

```Rust
use std::path::Path;

fn main() {
  let dir_path = Path::new("my_directory");
  if dir_path.is_dir() {
    println!("The directory exists!");
  } else {
    println!("The directory does not exist");
  }
}
```

אם תיקיית "my_directory" קיימת, המסר "The directory exists!" יודפס למסך. אחרת, יודפס "The directory does not exist". כעת נראה דוגמה נוספת של תרגיל שבו אנו בודקים אם אנו יכולים ליצור תיקייה עם אותו השם של התיקייה שבדקנו אם היא קיימת:

```Rust
use std::fs;

fn main() {
  let dir_path = Path::new("my_directory");
  if dir_path.is_dir() {
    println!("Can't create directory, it already exists");
  } else {
    fs::create_dir(dir_path).unwrap();
    println!("Directory created successfully");
  }
}
```

במקרה זה, אם התיקייה "my_directory" קיימת, יודפס מסר התראה שלא ניתן ליצור את התיקייה כיוון שהיא כבר קיימת. אחרת, התיקייה תיווצר בהצלחה וידפס מסר ההודעה "Directory created successfully".

## בירור מעמיק

ניתן גם לבדוק אם תיקייה קיימת באמצעות הפונקציות `Path::exists()` ו-`Path::is_file()`. פונקציית `Path::exists()` מחזירה בוליאני המציין האם ה