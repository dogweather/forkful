---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Go: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
קידוד המחרוזת לאותיות קטנות מושג בתכנות שמחמיר כאשר פונקציה מחזירה גרסה של מחרוזת בה כל האותיות היו בצורה קטנה. כמה מהסיבות לביצוע פעולה זו הן אחידות בנתונים, קלות בהשוואות בין מחרוזות ורגישות לאותיות גדולות וקטנות.

## איך לעשות:
```Rust
let s = "Hello, Rust Programmer!";
let lower_case = s.to_lowercase();
println!("{}", lower_case);
```
דוגמת הפלט:
```
hello, rust programmer!
```

## צלילה עמוקה
המתכנתים של Rust אמצו את הפונקציה `to_lowercase` ממנהג שקיים כבר מימי שפת C, על מנת להפעיל אותו גם בשפת Rust. ישנם חלופות לפונקציה הזו, אך הן יותר מסובכות לשימוש ולא תומכות בכל קידומת Unicode. הפונקציה `to_lowercase` עוברת על כל אותיות של המחרוזת ומחליפה לאות קטנה באמצעות כללים של Unicode Case Mapping.

## ראו גם
1. [תיעוד Rust של to_lowercase](https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase)
2. [כללים של Unicode Case Mapping](https://unicode.org/reports/tr21/)
3. [דיון ב StackOverflow על שימוש ב to_lowercase](https://stackoverflow.com/questions/26321592/how-to-convert-a-string-to-lowercase-in-rust)