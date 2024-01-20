---
title:                "מחיקת תווים התואמים לתבנית"
html_title:           "Elixir: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה ולמה?
מחיקת תווים התואמים לתבנית היא פעולה שבה משתמשים בשיטה של התאמה לדפוס ליינוק רשימת תווים. אנו מקיימים זאת כדי לייסר את הניתוח של מחרוזות - לחלקן ליחידות מידע, למנוע תווים לא רצויים ולעבד אותם בצורה מהירה יותר.

## כיצד ל:
```Rust
let s = "Hello, World!";
let result = s.chars().filter(|&c| c != ',').collect::<String>();
println!("{}", result);
```

הפלט:

```Rust
Hello World!
```

## איתוד מעמיק
מחיקת תווים התואמים לתבנית אפשרת בשפות תכנות רבות, נעשתה הרבה שימוש לצורך ניתוח אמין של מחרוזות. בלי היכולת למחוק תווים כחלק מתהליך הניתוח, חלק ממשימות העיבוד של מחרוזות היו פחות יעילות.

הפעולה היא קטנה יחסית, אך חשובה: היא פשוטה מבחינה תכניתית אך נצרכת רענונים במספר גישות ובשפות שונות, מאוד מגוונת.

היום אנו יכולים למנוע תווים באמצעות שיטת `filter()` ושיטת איסוף למחרוזת חדשה ב-Rust, אך זה לא היה נכונה תמיד.

## ראה גם 
1. [חיברן הכלאים של Rust](https://doc.rust-lang.org/book/ch08-02-strings.html)
2. [המדריך השלם לתכנות Rust](https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-lifetime-misconceptions.html)