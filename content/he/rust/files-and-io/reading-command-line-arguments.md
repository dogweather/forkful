---
date: 2024-01-20 17:58:06.262921-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1\u05E2\u05D1\
  \u05E8, \u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05E8\u05D2\u05D5\u05DE\u05E0\u05D8\
  \u05D9\u05DD \u05D4\u05D9\u05D9\u05EA\u05D4 \u05DE\u05D1\u05D5\u05E6\u05E2\u05EA\
  \ \u05D1\u05E9\u05E4\u05D5\u05EA \u05DB\u05DE\u05D5 C \u05D3\u05E8\u05DA \u05E4\u05D5\
  \u05E0\u05E7\u05E6\u05D9\u05D9\u05EA `main` \u05E2\u05DD `argc` \u05D5-`argv`. \u05D1\
  \u05E8\u05D0\u05E1\u05D8 \u05D6\u05D4 \u05DE\u05EA\u05DE\u05DE\u05E9\u05E7 \u05D9\
  \u05D5\u05EA\u05E8 \u05D1\u05D8\u05D5\u05D1 \u05D8\u05E2\u05DD \u05E2\u05DD `env::args`.\
  \ \u05D9\u05E9\u05E0\u05DF\u2026"
lastmod: '2024-04-05T22:50:53.242695-06:00'
model: gpt-4-1106-preview
summary: "\u05D1\u05E2\u05D1\u05E8, \u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05E8\u05D2\
  \u05D5\u05DE\u05E0\u05D8\u05D9\u05DD \u05D4\u05D9\u05D9\u05EA\u05D4 \u05DE\u05D1\
  \u05D5\u05E6\u05E2\u05EA \u05D1\u05E9\u05E4\u05D5\u05EA \u05DB\u05DE\u05D5 C \u05D3\
  \u05E8\u05DA \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D9\u05EA `main` \u05E2\u05DD\
  \ `argc` \u05D5-`argv`."
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E4\u05E8\u05DE\u05D8\u05E8\u05D9\u05DD\
  \ \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4"
weight: 23
---

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
