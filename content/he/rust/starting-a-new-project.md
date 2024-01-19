---
title:                "התחלת פרויקט חדש"
html_title:           "Clojure: התחלת פרויקט חדש"
simple_title:         "התחלת פרויקט חדש"
programming_language: "Rust"
category:             "Rust"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

## מה ולמה?
חל את הפרויקט החדש שלך, זו הנקודה בה אתה מתחיל את קוד ה- Rust שלך מאפס. מתכנתים מתחילים פרויקטים חדשים כדי לגשת למטרה מסוימת ולפתור בעיה מסוימת, בעזרת השפה, הספריות והדרכים שהם יודעים ואוהבים.

## איך לעשות:
יצירת פרויקט חדש ב- Rust היא תהליך פשוט. תחילה, מתקינים את Rust ערכת הכלים (rustup). כאשר ההתקנה מולחמת, אתה יכול להתחיל ביצירת הפרויקט שלך.

```Rust
$ rustup update
$ cargo new my_project
$ cd my_project
```

אתה יכול לראות את הקובץ `main.rs` שנוצר, שמכיל קוד Rust שמדפיס "Hello, world!".

```Rust
fn main() {
    println!("Hello, world!");
}
```

וכשאתה מריץ את הקוד, יודפס "Hello, world!".

```Rust
$ cargo run
   Compiling my_project v0.1.0 (/path/to/my_project)
    Finished dev [unoptimized + debuginfo] target(s) in 0.32s
     Running `target/debug/my_project`
Hello, world!
```

## הצצה למטה:
עם הזמן, Rust התפתח להיות אחת מהשפות המועדפות ביותר עבור מערכות נמוכות הרמה, פרויקטים בינוניים וגדולים, ובקוד ביצועים גבוהים. ייתרונותיו של Rust מול שפות אחרות כוללים את הביצועים הגבוהים, את הניהילת זיכרון שטובה, ואת המערכת המסוגלת למניעה של הרבה סוגים של באגים. 

באותו זמן, קיימות גם שפות תחליפיות כמו C++ וGo, שמספקות יתרונות תחרותיים. אך כשאנחנו מדברים על ניהילת זיכרון, Rust לא מעט לעזוב שלל באגים הקשורים לזיכרון בחוץ.

## ראה גם:
1. [Rust Documentation](https://doc.rust-lang.org/): המקום ללמוד על השפה, התחביר והספריות.
2. [Rust by Example](https://doc.rust-lang.org/stable/rust-by-example/): מדחיק ביותר ללמידת Rust, עם דוגמאות קוד מלאות.
3. [The Rust Programming Language](https://doc.rust-lang.org/book/): הספר הרשמי ללמידת Rust, עם הפרקים השונים שעוסקים בתחומים השונים של השפה.