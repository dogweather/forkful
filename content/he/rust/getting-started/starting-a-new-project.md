---
date: 2024-01-20 18:05:16.355496-07:00
description: "\u05D4\u05EA\u05D7\u05DC\u05EA \u05E4\u05E8\u05D5\u05D9\u05E7\u05D8\
  \ \u05D7\u05D3\u05E9 \u05D1\u05E8\u05D0\u05E1\u05D8 \u05D6\u05D4 \u05DB\u05DE\u05D5\
  \ \u05DC\u05E7\u05D7\u05EA \u05E2\u05DE\u05D5\u05D3 \u05D1\u05DC\u05D1\u05DF \u05D5\
  \u05DC\u05D4\u05EA\u05D7\u05D9\u05DC \u05DC\u05E6\u05D9\u05D9\u05E8 \u05E2\u05DC\
  \u05D9\u05D5 \u05D0\u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05E6\u05D9\u05D4 \u05E2\
  \u05DD \u05D4\u05DE\u05D7\u05E9\u05D1. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\
  \ \u05DE\u05EA\u05D7\u05D9\u05DC\u05D9\u05DD \u05E4\u05E8\u05D5\u05D9\u05E7\u05D8\
  \u05D9\u05DD \u05D7\u05D3\u05E9\u05D9\u05DD \u05DB\u05D3\u05D9 \u05DC\u05D9\u05E6\
  \u05D5\u05E8 \u05EA\u05D5\u05DB\u05E0\u05D4, \u05DC\u05DC\u05DE\u05D5\u05D3 \u05D8\
  \u05DB\u05E0\u05D5\u05DC\u05D5\u05D2\u05D9\u05D5\u05EA, \u05D0\u05D5\u2026"
lastmod: '2024-02-25T18:49:37.228304-07:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05EA\u05D7\u05DC\u05EA \u05E4\u05E8\u05D5\u05D9\u05E7\u05D8 \u05D7\
  \u05D3\u05E9 \u05D1\u05E8\u05D0\u05E1\u05D8 \u05D6\u05D4 \u05DB\u05DE\u05D5 \u05DC\
  \u05E7\u05D7\u05EA \u05E2\u05DE\u05D5\u05D3 \u05D1\u05DC\u05D1\u05DF \u05D5\u05DC\
  \u05D4\u05EA\u05D7\u05D9\u05DC \u05DC\u05E6\u05D9\u05D9\u05E8 \u05E2\u05DC\u05D9\
  \u05D5 \u05D0\u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05E6\u05D9\u05D4 \u05E2\u05DD\
  \ \u05D4\u05DE\u05D7\u05E9\u05D1. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\
  \u05EA\u05D7\u05D9\u05DC\u05D9\u05DD \u05E4\u05E8\u05D5\u05D9\u05E7\u05D8\u05D9\u05DD\
  \ \u05D7\u05D3\u05E9\u05D9\u05DD \u05DB\u05D3\u05D9 \u05DC\u05D9\u05E6\u05D5\u05E8\
  \ \u05EA\u05D5\u05DB\u05E0\u05D4, \u05DC\u05DC\u05DE\u05D5\u05D3 \u05D8\u05DB\u05E0\
  \u05D5\u05DC\u05D5\u05D2\u05D9\u05D5\u05EA, \u05D0\u05D5\u2026"
title: "\u05D4\u05EA\u05D7\u05DC\u05EA \u05E4\u05E8\u05D5\u05D9\u05E7\u05D8 \u05D7\
  \u05D3\u05E9"
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
