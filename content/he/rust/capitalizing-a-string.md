---
title:                "הפיכת מחרוזת לאותיות רישיות"
date:                  2024-01-19
html_title:           "Bash: הפיכת מחרוזת לאותיות רישיות"
simple_title:         "הפיכת מחרוזת לאותיות רישיות"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
הפיכת מחרוזת לאותיות רישיות אומר לשנות את כל האותיות במחרוזת לאותיות גדולות. תוכניתנים עושים זאת להדגשה או להתאמה לקונבנציות קוד.

## איך לעשות:
ב־Rust, אפשר להפוך מחרוזת לאותיות רישיות בעזרת המתודה `to_uppercase`. הנה דוגמה:

```rust
fn main() {
    let lowercase = "shalom, olam!";
    let uppercase = lowercase.to_uppercase();
    println!("{}", uppercase);
}
```

פלט דוגמה:

```
SHALOM, OLAM!
```

## צלילה עמוקה:
המתודה `to_uppercase` מופיעה בסטנדרט של ראסט ומשתמשת בתקנים של Unicode להמרה נכונה של האותיות. יש גם אלטרנטיבות, כמו `to_ascii_uppercase`, שמטפלת רק באותיות לטיניות. השימוש ב`to_uppercase` יכול להיות יקר זמן-ריצה ביחס ל`to_ascii_uppercase` בגלל התמיכה בינלאומית. בעברית, למשל, לא תמיד יהיה שינוי משמעותי, אך בשפות אחרות ההבדלים יכולים להיות משמעותיים.

## ראה גם:
- [Rust Documentation for `to_uppercase`](https://doc.rust-lang.org/std/primitive.str.html#method.to_uppercase)
- [Unicode Case Mapping](https://www.unicode.org/reports/tr21/tr21-5.html)
- [Rust by Example on Custom Types/Enums](https://doc.rust-lang.org/rust-by-example/custom_types/enum.html)
