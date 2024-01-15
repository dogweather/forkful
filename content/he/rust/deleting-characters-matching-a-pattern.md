---
title:                "מחיקת תווים התואמים לתבנית"
html_title:           "Rust: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## למה

אנשים משתמשים בניווט באתר של הכוונה של ה מחוק תווים התאמת תבנית בכדי לסנן ולשפר את תוצאות החיפוש שלהם.

## כיצד לעשות זאת

כאשר אתם משתמשים בשפת ראסט, ישנן מספר אפשרויות למחיקת תווים התואמים תבנית. נוכל להשתמש בפונקציות כמו `replace()` ו `trim()` כדי למחוק תווים מסוימים ממחרוזות. להלן דוגמאות של כיצד להתגבר על תבניות בתוך קוד רסט, כולל הפלט המשוער עבור כל דוגמה.

```Rust
//משתנה מחרוזת מכיל טקסט עם תווים כפולים ורווחים לא נחוצים
let text = "Heeeeellllooooo Wo000000rld 999999";
//השתמשו בפונקציית `replace()` כדי להסיר את כל התווים שאתם רוצים למחוק
let mut new_text = text.replace("e", "");
//הדפיסו את התוצאות כדי להראות את השינויים
println!("{}", new_text);
//לפלט היא "Hlllllooooo Wo000000rld 999999"
```

```Rust
//משתנה מחרוזת מכיל טקסט עם תוים כפולים ורווחים לא נחוצים
let text = "Helloooo World";
//השתמשו בפונקציית `trim()` כדי למחוק את התווים הלא נחוצים
let mut new_text = text.trim();
//הדפיסו את התוצאות כדי להראות את השינויים
println!("{}", new_text);
//לפלט היא "Helloooo World"
```

## מטפל עמוק

כאשר אנחנו מחפשים תבניות במחרוזות, אתם יכולים להשתמש בפונקציות על מנת לשפר את הסניף שלכם. נוכל להשתמש בתבניות רגולריות ובמבני נתונים כדי לבדוק ולמחוק תווים על פי יותר מדויק תיעוד.

## ראה גם

- [The Rust Programming Language](https://www.rust-lang.org)
- [Rust by Example](https://doc.rust-lang.org/rust-by-example)
- [Rust Regular Expressions Tutorial](