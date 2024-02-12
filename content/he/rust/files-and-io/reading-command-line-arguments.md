---
title:                "קריאת פרמטרים משורת הפקודה"
aliases:
- he/rust/reading-command-line-arguments.md
date:                  2024-01-20T17:58:06.262921-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת פרמטרים משורת הפקודה"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת ארגומנטים משורת הפקודה היא לקחת טקסט שמשתמש מזין כשהוא מריץ תוכנית. תכניתאים עושים זאת כדי לאפשר גמישות והתאמה אישית בריצת התוכנית דרך מספר סצנריות שונות.

## איך לעשות:
איך קוראים לארגומנטים? קל:
```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    
    // כדי להדפיס את כל הארגומנטים:
    println!("{:?}", args);
    
    // נניח שאתה רוצה להשתמש בארגומנט הראשון:
    if args.len() > 1 {
        println!("ארגומנט 1: {}", args[1]);
    }
}
```

אם תריץ `cargo run -- שלום עולם` תראה:
```
["target/debug/my_app", "שלום", "עולם"]
ארגומנט 1: שלום
```

## טבילה עמוקה:
בעבר, קריאת ארגומנטים הייתה מבוצעת בשפות כמו C דרך פונקציית `main` עם `argc` ו-`argv`. בראסט זה מתממשק יותר בטוב טעם עם `env::args`.

ישנן ספריות שמפשטות את קריאת ארגומנטים ופרשנותם, כמו `clap` ו-`structopt`, אבל הדוגמא לעיל מראה כיצד לעשות זאת בסטנדרטי עם מודול `std::env`.

## ראה גם:
- תיעוד Rust לגבי קלט משורת פקודה: https://doc.rust-lang.org/std/env/fn.args.html
- ספריית `clap` לניתוח ארגומנטים: https://crates.io/crates/clap
- ספריית `structopt` לניתוח ארגומנטים מובנה באמצעות סטרקטים: https://crates.io/crates/structopt
- Rust by Example - Command Line Arguments: https://doc.rust-lang.org/rust-by-example/std_misc/arg/matching.html
