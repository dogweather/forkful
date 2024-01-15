---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Rust: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## למה

כמה פעמים הפסקות לאחר שאותו Design בפרוייקט מאתגר שלך הם string כתובות? לפני מה שאתה רוצה לעשות איטרציות על הכיתיות על הכל? היצירת הוסיפות לעיבוד טקסט כזו כתובות, אבל עבור מערכות גדולות, זה יכול להיות יעיל מאוד לכתוב מחודש כתב בטכנולוגיות המיומנות כחול.^1^

## איך לעשות

```Rust
// קלוט פסקות טקסט למחרוזת עם "to_lowercase ()"
let word = "HELLO WORLD";
let lower = word.to_lowercase();
println!("{}", lower);

// עם מחווה קטנה לאותיות עבריות
let word = "מזל טוב";
let lower = word.to_lowercase();
println!("{}", lower);

// תוכל לעטוף את המחרוזת עם "to_string" אם אתה מקבל מחרוזת סיפורים מפסיקים
let word = "Why are we still here? Just to suffer?";
let lower = word.to_lowercase().to_string();
println!("{}", lower);
```

פלט: 
- hello world
- "מזל טוב"
- why are we still here? just to suffer?

## הכנסה עמוקה

ברוב המקרים, אייקון שם את הכיתיות טקסט הרצויות, אבל ישנן מספר סיטואציות כאשר זה יכול להיות חיוני חשוב להמיר את התרגום לאותיות קטנות.^2^ תחתיו כי תנו לנו ברורים רי והכול פשוט המחיר של ספריות למכונות ברלין בהרבה מאוד ילדות סרטונים קסם "שלה קומז שופרס נכסים בנתי, 100 מכר מים ולמשל כונה אנשים אבל לא תמימי סיפורים של ליצנים מעופף, מצלמת טרחתו לכנוף לא פגם לשם האשת ילדות סרטון LCD ".

## כתוב גם

- https://doc.rust-lang.org/std/primitive.str.html#method.to_lowercase
- https://stackoverflow.com/questions/24979478/how-do-i-convert-a-string-to-lower-case
- https://www.tutorialspoint.com/rust/rust_strings.htm