---
title:                "Rust: לכתיבה אל תוך שגיאת תקן"
simple_title:         "לכתיבה אל תוך שגיאת תקן"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## למה
כתיבה לפלט שגיאה סטנדרטי נחשבת לכלי חשוב בתוך תהליך הקודם לכתיבת שגיאות בתוכניות. בדרך זו, אפשר לזהות ולטפל בשגיאות כמעט בזמן אמת ולהקטין את כמות הזמן והמאמץ הנדרשים לטיפול בהם.

## איך לעשות זאת
בדוגמאות הקוד והפלט שיפורטו בהמשך, אנחנו נשתמש בשפת תכנות Rust כדי להדגים איך לבצע כתיבה לפלט שגיאה סטנדרטי. נתחיל עם פונקציה פשוטה שמקבלת מספר כפרמטר ומדפיסה את הערך שלו לפלט שגיאה. לשם כך, נשתמש בפונקציה `eprintln!` מתוך הספרייה `std`.

```Rust
fn print_to_stderr(number: i32) {
    eprintln!("The number is: {}", number);
}
```

כעת, נדרוס את הפונקציה ונעביר לה ערך של משתנה שלא קיים.

```Rust
let non_existing_number = 100;
print_to_stderr(non_existing_number);
```

הפלט של הקוד הנ"ל יהיה:

```
The number is: 100
```

ניתן להעביר לפונקציה זו גם מחרוזת כפרמטר ולהדפיס גם הודעות שגיאה ידניות לפלט.

```Rust
fn print_to_stderr(string: &str) {
    eprintln!("There was an error in the program: {}", string);
}

let error_message = "Error occurred.";
print_to_stderr(error_message);
```

הפלט של הקוד הנ"ל יהיה:

```
There was an error in the program: Error occurred.
```

## מגע מעמיק
הכתיבה לפלט שגיאה סטנדרטי יכולה לעזור גם במקרים שבהם נמצא שגיאה ספציפית בתוך התכנית ואנחנו רוצים לדעת מה היא בדיוק. במקרה זה, ניתן להשתמש במקרה של אינטרפרטציה של שגיאות מתוך המחרוזת ולתת למכונה לחזור על הפעולה שביצעה את השגיאה, כך שנוכל להבין מה