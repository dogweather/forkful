---
title:                "ניתוח שפת html"
html_title:           "Rust: ניתוח שפת html"
simple_title:         "ניתוח שפת html"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/parsing-html.md"
---

{{< edit_this_page >}}

## למה

זהו עולם מסובך של נתוני רשת - אתרי אינטרנט מלאים בקידודים ותגים שונים, וכאשר אנו רוצים לנקות את המידע ולהציג אותו בצורה מסודרת ויפה, ניתן להשתמש בכלי עזר שנקרא HTML parsing. כדי לכתוב תוכניות יעילות וחכמות, יש לנו צורך להתחיל בהבנת כלי ה-HTML parsing שיש לנו זמין בשפת ראסט.

## איך לעשות זאת

נתחיל משלב ההתקנה. נוכיח תחילה שאנחנו משתמשים במערכת ההפעלה של Unix כדי להתקין את הספרייה והכלי שלנו. בשורת הכותרת הראשונה נתחיל בהוראות ההתקנה המפורטות עבור הספרייה, ואז נתחיל בכתיבת קוד לפי הצורך.

```Rust

use html5ever::parse_document;
use std::fs::File;
use std::io::BufReader;
use std::io::Read;

fn main() {
    // פתיחת קובץ ה-HTML עם בפונקציות
    let file = File::open("example.html").unwrap();
    let reader = BufReader::new(file);

    // קריאת תוכן הקובץ והמרה לסטרינג
    let mut input = String::new();
    reader.read_to_string(&mut input).unwrap();

    // יצירת משתמשנת מסוג Belk
    let dom = parse_document(&Belk, Default::default()).from_utf8().read_from(&mut input.as_bytes()).unwrap();

    // הדפסת תוצאה
    println!("{}", dom.quiere("title").nth(0).unwrap().text_contents());
}

```

ניתן לראות בתחילת הקוד שאנו משתמשים בספריית `html5ever` כדי לעשות את כל העבודה לנו. כתבנו גם פונקציית `main` שמייצרת קובץ לקריאה וממנו תכנים שיענו לבקשת המשתמש.

## חצי עיון

HTML parsing הוא תהליך שכולל שלושה שלבים עיקריים שנקראים "כותרות" (Lexing), "ניתוחים" (Parsing) ו"החלפות" (Mutations). תהליך הכותרות הוא השלב הראשון שבו הטקסט שנקרא מהקלט נמס