---
title:                "Rust: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

המימוש של הפונקציה להמיר מחרוזת לאותיות קטנות הוא נחשב כקל במגוון שפות תכנות אחרות, אך זה יכול להיות קצת מאתגר כאשר מדובר בשפת ראסט. אז למה בכלל להתעסק בהמרת מחרוזת לאותיות קטנות? התשובה היא פשוטה - בכדי לקבל את המילים והתווים הנכונים בפורמט תקין, בדוק אם יש צורך להשתמש בהן בתוך אלגוריתמים או להשוות ביניהן.

## איך לעשות

הנה דוגמאות של קוד המדגימות איך להמיר מחרוזת לאותיות קטנות בשפת ראסט:

```Rust

fn to_lower_case(string: &str) -> String {
    let mut result = String::new();

    for c in string.chars() {
        if c.is_uppercase() {
            result.push(c.to_lowercase().next().unwrap());
        } else {
            result.push(c);
        }
    }

    return result;
}

// כאן נקבל את הפלט "hello world" כאשר הקלט הוא "Hello World"
print!("{}", to_lower_case("Hello World"));

```

הנה עוד דוגמא עם פונקצית המשמשת להמיר לאותיות קטנות שנמצאת בספריית התקנים של ראסט:

```Rust

use std::collections;

fn to_lower_case(string: String) -> String {
    let mut map = collections::HashMap::new();

    map.insert(String::from("A"), String::from("a"));
    map.insert(String::from("B"), String::from("b"));
    map.insert(String::from("C"), String::from("c"));
    // וכן הלאה עד שמיתוגרש אף אות לאותה המקבילה שלה באותיות קטנות

    let result: Vec<_> = string
        .chars()
        .map(|c| match map.get(&c.to_string().to_uppercase()) {
            Some(v) => v.to_string(),
            None => c.to_string(),
        })
        .collect();
    return result.join("");
}

// כאן נקבל את הפלט "hello world" כאשר הקלט הוא "Hello World"
print!("{}", to_lower_case("Hello World".to_string()));

```

## טיול עמוק

הפונקציה להמרת מחרוזת לאותיות קטנות נחשבת לאחת מבורך מאוד שבמגוון השפות התכנות. ברמת הקוד, זהו כמעט תמיד ניתן לממש בצורה קלה ומהירה. אבל הדרך שבה סביר למ