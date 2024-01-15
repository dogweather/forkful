---
title:                "המרה למחרוזת גדולות"
html_title:           "Rust: המרה למחרוזת גדולות"
simple_title:         "המרה למחרוזת גדולות"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## למה

אנשים משתמשים בפעולת כיתוביות של שפת Rust בכדי לשדרג את מחרוזות התווים שלהם. זה מאפשר להם לטפל במחרוזות בצורה מקצועית ופשוטה, מתוך כך מבטיחים קוד איכותי יותר וקל יותר לתחזוקה.

## כיצד עושים זאת

הנה כמה דוגמאות של איך לכתוב טיפוס Rust כדי לכיתב מחרוזות תווים:

```rust
fn capitalize(string: &str) -> String {
    return string.to_uppercase();
}

fn main() {
    let input_string = "hello world!";
    let capitalized_string = capitalize(input_string);
    println!("Original string: {}", input_string);
    println!("Capitalized string: {}", capitalized_string);
}
```

פלט:

```shell
Original string: hello world!
Capitalized string: HELLO WORLD!
```

הפעולה "to_uppercase()" משנה את כל התווים במחרוזת לאותיות גדולות. בכל פעם שתרצו לכנות לפעולה את הפונקציה "capitalize()", תוכלו להשתמש בפונקציה הזו כדי להכין מחרוזת עם כל האותיות הגדולות.

## חפירה עמוקה

כדי להבין טוב יותר את הפעולה "to_uppercase()" וכיצד היא משנה את מחרוזת הקלט, נצפה בפונקציה המעתיקה את הפעולה ונבדוק קצת את הלוגיקה שלה:

```rust
fn to_uppercase(string: &str) -> String { 
    let mut result = String::new();

    for c in string.chars() {
        // Check if current character is a lowercase letter
        if c.is_ascii_lowercase() {
            // Convert lowercase to uppercase by subtracting 32 from its ASCII value
            let uppercase_char = (c as u8 - 32) as char;
            // Add the uppercase character to the result string
            result.push(uppercase_char);
        } else {
            // If the character is already an uppercase letter, add it to the result as is
            result.push(c);
        }
    }

    return result; 
}
```

בכדי להבין מתוך מה כיתוביות של Rust משמשת לטיפוס שלהם כמו "char", עלינו להבין שאפשר לנגוח ולכתוב על הנתונים הללו בצורה ממוחשבת. במילים אחרות, הלוגיקה של להכניס אותיות גדולות לנתונים זהה לאיך המכשור ממיר אותיות גדולות וקטנות במחשב.

## ראו גם

- [מסמ