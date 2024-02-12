---
title:                "התחלת פרויקט חדש"
aliases:
- /he/rust/starting-a-new-project.md
date:                  2024-01-20T18:05:16.355496-07:00
model:                 gpt-4-1106-preview
simple_title:         "התחלת פרויקט חדש"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

## מה ולמה?
התחלת פרויקט חדש בראסט זה כמו לקחת עמוד בלבן ולהתחיל לצייר עליו אינטראקציה עם המחשב. מתכנתים מתחילים פרויקטים חדשים כדי ליצור תוכנה, ללמוד טכנולוגיות, או לבדוק רעיון. 

## איך לעשות:
כדי להתחיל פרויקט חדש בראסט, תשתמש ב-Cargo, המנהל הרשמי של מאגרי הקוד של ראסט. הנה איך אתה עושה את זה:

```Rust
// התקן את Cargo, אם אין לך אותו עדיין.
// בטרמינל שלך הקלד:
$ curl https://sh.rustup.rs -sSf | sh

// לאחר ההתקנה, צור פרויקט חדש באמצעות:
$ cargo new my_project
$ cd my_project

// עכשיו תראה מבנה התיקיות הבסיסי:
$ tree

.
├── Cargo.toml
└── src
    └── main.rs

// כדי לרוץ את הפרויקט:
$ cargo run

   Compiling my_project v0.1.0 (/path/to/my_project)
    Finished dev [unoptimized + debuginfo] target(s) in 0.0 secs
     Running `target/debug/my_project`

Hello, world!
```
ברכותיי, יש לך פרויקט ראסט חדש.

## צלילה עמוקה
שיחת הפקודה `cargo new` מיד אחרי התקנה של Cargo הוא מעשה קלאסי בקרב תכנתי ראסט. Cargo הוא כלי שמאתחל פרויקט עם כל מה שצריך כדי להתחיל לקודד מיד: קובץ `Cargo.toml` לניהול תלותים וקונפיגורציה ותיקיית `src` עם `main.rs` בתוכה – נקודת ההתחלה של כל קוד ראסט. לפני ש-Cargo היה, תכנתי ראסט היו צריכים להגדיר הרבה מהדברים האלה ידנית. אלטרנטיבות בנבנה בקהילה מאפשרות לעתים פיתוחים מתקדמים יותר למי שצריך – אבל Cargo הוא המכנה המשותף שכולם יודעים ושמים.

## ראה גם
- [תיעוד ראסט](https://www.rust-lang.org/learn) – הבסיס לכל מה שקשור בראסט.
- [תיעוד קרגו](https://doc.rust-lang.org/cargo/) – על מנת להעמיק בשימוש בקרגו.
- [The Rust Programming Language](https://doc.rust-lang.org/book/) – הספר הכי מומלץ ללמוד ראסט מתחילת דרך עד מתקדם.
