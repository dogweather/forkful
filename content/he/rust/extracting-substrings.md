---
title:                "חילוץ תת-מחרוזות"
html_title:           "Bash: חילוץ תת-מחרוזות"
simple_title:         "חילוץ תת-מחרוזות"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## מה ולמה?

חילוץ תת-מחרוזות הוא פעולה שבה אנו בוחרים קטע ממחרוזת קיימת. מתכנתים עושים זאת כדי לקבל תת-חלק מתוך מחרוזת בהם הם מעוניינים.

## איך לעשות:

```Rust
fn main() {
    let text = "שלום, עולם!";
    let hello = &text[0..6];
    println!("{}", hello);  // Prints "שלום"
}
```

התוצאה של זה תהיה "שלום".

```Rust
fn main() {
    let text = "שלום, עולם!";
    let world = &text[7..12];
    println!("{}", world);  // Prints "עולם"
}
```

הפלט של הדוגמה הזו יהיה "עולם".

## בפרטים עמוקים:

המטרה ההיסטורית של חילוץ תת-מחרוזות הייתה לאפשר למתכנתים לטפל במעט מאוד דאטא גם אם אחת מהמשתנים שלהם היא מחרוזת ארוכה מאוד. חלופותינו כוללות שימוש בפעולות מחרוזת אחרות כמו "מצא" או "חתך". בראסט, אנחנו מניחים את התת-מחרוזת שאנחנו מעוניינים לחלץ מתוך מתמטיקת מערך, מה שמשמעתו שחילוץ תת-מחרוזות הוא פשוט דפיקה לכתובת מיקום מסוימת.

## ראה גם:

האם אתה מעוניין ללמוד עוד? הנה כמה מקורות:

- [הספר הרשמי של Rust](https://doc.rust-lang.org/book/ch08-02-strings.html)
- [המדריך למתכנת הראסט](https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-lifetime-misconceptions.html)
- [דיון בפורום של Rust](https://users.rust-lang.org/t/how-to-get-a-substring-of-a-string/1351)