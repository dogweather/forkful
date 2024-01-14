---
title:                "Rust: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# למה

ישנם מצבים רבים שבהם נדרש למחוק תווים המתאימים לתבנית מסוימת בתכנות בשפת Rust. כתבה זו יכולה להעניק לך כלים והבנה כיצד לבצע פעולה זו בדרך הטובה ביותר.

## כיצד לבצע פעולה זו

כאשר מעוניינים למחוק תווים המתאימים לתבנית מסוימת, ניתן להשתמש בפונקציה "replace" של שפת Rust. לדוגמה, אם נרצה למחוק את כל התווים הלא נראים בשחורים בתמונה, נוכל להשתמש בקוד הבא בתוך ה"```Rust ... ```":

```Rust
let str = "Hello, world!";
let new_str = str.replace("l", "");
println!("{}", new_str); // Output: Heo, word!
```

## להתעמק

ניתן להשתמש גם בתנאים כדי לבצע פעולה זו על תווים מסוימים בלבד. לדוגמה, נוכל להשתמש בקוד הבא כדי למחוק רק את התווים "l" ו"o" מהתוכן:

```Rust
let str = "Hello, world!";
let new_str = str.replace(|c| match c {
    'l' | 'o' => false,
    _ => true,
}, "");
println!("{}", new_str); // Output: Hel, wrd!
```

# ראה גם

- [מדריך רשמי על פעולה של הפונקציה "replace" בשפת Rust](https://doc.rust-lang.org/std/string/struct.String.html#method.replace)
- [תצוגה לעומק על תנאים בשפת Rust](https://doc.rust-lang.org/book/ch18-03-pattern-syntax.html#matching-with-partial-value-bindings)