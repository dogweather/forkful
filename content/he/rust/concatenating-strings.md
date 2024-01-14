---
title:    "Rust: חיבור מחרוזות"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## למה

למינוייית סימני מחרוזות היא דרך נוחה לשלב מחרוזות יחד וליצור מחרוזת אחת גדולה.

## איך לעשות זאת

```Rust
let first_name = "דפנה";
let last_name = "כהן";
let full_name = format!("{} {}", first_name, last_name);

println!("שלום, אני {}", full_name);
```

הפלט:

```Rust
שלום, אני דפנה כהן
```

אפשר גם להתמש באופרטור `+` כדי למזג מחרוזות:

```Rust
let hello = "שלום";
let name = "דפנה";
let message = hello + ", " + name + "!";

println!("{}", message);
```

הפלט:

```Rust
שלום, דפנה!
```

## חפירה עמוקה

כשמשתמשים בפונקציית `format` במחרוזות מוגדרים מראש כמערך, אפשר להשתמש באותו אינדקס כמפתח למחרוזת במערך כדי למזג אותן:

```Rust
let names = ["דפנה", "אלכס", "תמר"];
let sentence = "{0} ו{1} נפגשו עם {2}.".format(&names[0], &names[1], &names[2]);

println!("{}", sentence);
```

הפלט:

```Rust
דפנה ואלכס נפגשו עם תמר.
```

## ראו גם

- [השתמש בפונקציית `format`](https://doc.rust-lang.org/std/fmt/#formatting-traits)
- [אופרטורים ברחבי Rust](https://doc.rust-lang.org/book/ch05-02-example-stacks.html#operators)