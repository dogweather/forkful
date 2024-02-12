---
title:                "חיפוש והחלפת טקסט"
aliases:
- /he/rust/searching-and-replacing-text.md
date:                  2024-01-20T17:58:54.176674-07:00
model:                 gpt-4-1106-preview
simple_title:         "חיפוש והחלפת טקסט"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
לחפש ולהחליף טקסט זה פעולה שמאפשרת למצוא מחרוזות בתוך טקסט ולהחליפם באחרים. תכנותים עושים את זה בשביל לעדכן קוד, לתקן שגיאות, או לשנות נתונים בפרויקט.

## How to: (איך לעשות:)
```Rust
fn main() {
    let text = "Hello, world! Programming in Rust is fun.";
    let search = "world";
    let replace_with = "there";

    let replaced_text = text.replace(search, replace_with);
    println!("{}", replaced_text);
}
```
פלט:
```
Hello, there! Programming in Rust is fun.
```

## Deep Dive (צלילה עמוקה)
בעולם התוכנה, פעולת החיפוש והחלפה הייתה חלק מעורכי טקסט מהימים הראשונים של תכנות. זה מכשיר חזק שיכול לחסוך זמן רב ולמנוע שגיאות. בראסט, `str::replace` היא פונקציה פשוטה וישירה לשימוש זה. חלופות כוללות ביטויים רגולריים עם crate `regex` לחיפושים מורכבים יותר, וגיימן למניפולציה של טקסט עם `str::split` ואיטרטורים. פנימית, פעולת החיפוש והחלפה מתבצעת על ידי סריקה של המחרוזת ובנייה מחדש של מחרוזת עם הערכים החדשים.

## See Also (ראה גם)
- [Rust std::str documentation](https://doc.rust-lang.org/std/str/)
- [Rust by Example: Strings](https://doc.rust-lang.org/rust-by-example/std/str.html)
- [`regex` crate documentation](https://docs.rs/regex/latest/regex/)
