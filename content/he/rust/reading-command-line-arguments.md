---
title:                "Rust: קריאת ארגומנטים מפקודת הפקודה"
simple_title:         "קריאת ארגומנטים מפקודת הפקודה"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# למה
רושם הקוד לאתחול פרמטרי קו פקודה הוא מנגנון חשוב בתכנות בג'ייסון לשתף, והוא יכול להיות מועיל בפתרון בעיות שונות בתכנות. קריאת פרמטרים מפקודת קו המפקדים מאפשרת למשתמש להתנתק טכלונוגית ולשנות את המשתנים ולקבל מידע נכון מהמשתמש.

# איך לקרוא פרמטרי קו פקודה בראסט
תחילה, נצטרך לייבא את המנגנון לשתף, כך שנוכל להשתמש בפונקציות שלו:

```Rust
use std::env;
```

לאחר מכן נוכל לקרוא את הפרמטרים שהועברו על ידי המשתמש באמצעות תפריט env::args ולהדפיס אותם:

```Rust
fn main() {
    let args: Vec<String> = env::args().collect();
    println!("{:?}", args);
}
```

כעת, נראה את הפלט של תוכנית זו כאשר נוסיף כמה פרמטרים בפקודת הקו:

```Rust
$ ./args_example hello world

["./args_example", "hello", "world"]
```

כדי לפרק את הפרמטרים שנמצאים בתוך משתנה מסוג Vec<String> ניתן להשתמש במתודות כמו .pop(), .next() ועוד.

# מעמק
כדי להשתמש בפרמטרי קו הפקודה בראסט בצורה יעילה יותר, ניתן להשתמש בספרייה נוספת כמו clap או structopt. ספריות אלו מאפשרות לנו להגדיר בקלות את הפרמטרים העזרים ולקבל קלט מהמשתמש בצורה נוחה ומסודרת.

# ראו גם
- https://doc.rust-lang.org/std/env/index.html
- https://crates.io/crates/clap
- https://crates.io/crates/structopt