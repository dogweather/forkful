---
title:                "מציאת אורך של מחרוזת"
html_title:           "Rust: מציאת אורך של מחרוזת"
simple_title:         "מציאת אורך של מחרוזת"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## למה
תכנות בשפת ראסט הוא דרך מהירה ויעילה להרחיב את יכולות התכנות שלך. אחת התכונות החשובות ביותר של ראסט היא היכולת למצוא את אורך של מחרוזת באופן יעיל ומהיר. זה מאפשר למתכנתים לעבוד בקלות עם מחרוזות ולעשות את הקוד שלהם יעיל יותר.

## איך לעשות זאת
תחילה, נקבל מחרוזת ונשמור אותה במשתנה. לדוגמה, נשמור את המחרוזת "שלום עולם" במשתנה "str". לאחר מכן, נשתמש בפונקציה "len" כדי למצוא את אורך המחרוזת. ניתן לעשות זאת בעזרת הקוד הבא תוך שימוש בשפת ראסט:

```rust
let str = "שלום עולם";
println!("אורך המחרוזת הוא {}", str.len());
```

בהפעלה של הקוד תקבלו את התוצאה הבאה:

```
אורך המחרוזת הוא 9
```

כעת תוכלו לשלב את פונקציית האורך בתוך קודים מורכבים יותר, כמו לדוגמה קבלת קלט מהמשתמש ובדיקת אם המחרוזת שהוזנה יכולה לשמור על כמה תנאים מסוימים. זהו למשל הקוד שאפשר ליצור:

```rust
use std::io; // import the "io" library

fn main() {
    println!("הזינו מחרוזת:");
    let mut input = String::new(); // create a mutable string variable for input
    io::stdin().read_line(&mut input).expect("קלט לא תקין"); // get user input and save it in "input"
    let length = input.trim().len(); // use the "len" function on input after removing whitespace

    if length > 5 {
        println!("המחרוזת הוכנסה היא ארוכה מדי");
    } else {
        println!("הכניסה שלכם היא בסדר");
    }
}
```

תוצאת הרצת הקוד יכולה להיות כזו:

```
הזינו מחרוזת:
שלום
הכניסה שהוכנסה היא חוקית
```

## לצעוד לתוכן
כעת שיש לנו יסודות טובים להכרת פונקצ