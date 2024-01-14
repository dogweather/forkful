---
title:    "Rust: המרת מחרוזת לאותיות קטנות"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## למה

תוכן בלבד: סיבה/סיבות למדוע מתאימים להתעסק בהמרת מחרוזת לאותיות קטנות.

## איך לעשות

תוכן בלבד:
 ```Rust
 fn main() {
     let string = "מחרוזת עם אותיות בגודל רב";
     let lowercase_string = string.to_lowercase();
     println!("מחרוזת באותיות קטנות: {}", lowercase_string);
 }
 ```
 פלט: 
 `מחרוזת באותיות קטנות: מחרוזת עם אותיות בגודל רב`

## חפירה עמוקה

תוכן בלבד:
 המרה לאותיות קטנות היא תהליך פשוט שבעזרתו ניתן לקרוא את כל התווים של מחרוזת ולהחליף אותם באותיות קטנות תואמות. כך, נוכל להשתמש במחרוזת עם אותיות ביניים או באותיות גדולות ולהמיר אותה למחרוזת עם אותיות קטנות במקרה הצורך.

## ראו גם

- [מדריך על מחרוזות ואופרטורים בשפת Rust](https://osherove.com/blog/2020/10/30/rust-strings-operators-guide)
- [תיעוד על הפונקציה to_lowercase ברשימת הפונקציות המובנות של Rust](https://doc.rust-lang.org/std/primitive.str.html#method.to_lowercase)
- [דוגמאות להמרת מחרוזת לאותיות קטנות עם שימוש במודול Strings בשפת Rust](https://turreta.com/2019/05/25/rust-how-to-convert-a-string-to-lowercase-using-the-strings-module/)