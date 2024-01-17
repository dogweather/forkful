---
title:                "הדפסת תוצאות ניתוח שגיאות"
html_title:           "Rust: הדפסת תוצאות ניתוח שגיאות"
simple_title:         "הדפסת תוצאות ניתוח שגיאות"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## מה ולמה?

הדפסת פלט דיבאג היא תהליך בו מפותחים מוסיפים פקודות בקוד שלהם כדי להדפיס מידע נוסף שיעזור להבין את תהליך הקוד ולזהות בעיות. תהליך זה נקרא גם "דיבאגינג" והוא חשוב בפיתוח תוכניות כדי לבצע תיקון תקלות ושיפורים.

## איך לעשות זאת:

דוגמאות קוד ופלט מסבירות יותר מידי מאלף מילים, לכן נשתמש בקוד ראסט (Rust) הבא כדי להדגים איך להדפיס פלט דיבאג כאשר מפתחים תוכניות בשפת ראסט.

```Rust
fn main() {
    let num1 = 5;
    let num2 = 10;
    println!("Debug output: {} + {} = {}", num1, num2, num1 + num2);
}
```

פלט הדיבאג של הקוד הנ"ל יוצא כך: 

```
Debug output: 5 + 10 = 15
```

ניתן להשתמש גם בפקודת `dbg!()` עבור פלט דיבאג קצר יותר, כך:

```Rust
fn main() {
    let num = 5;
    dbg!(num);
}
```

התוכנית שלנו תדפיס:

```
[src/main.rs:3] num = 5
```

## העממה עמוקה:

דיבאגינג הוא תהליך שמתבשל מאז שהיו בני אדם שמתחילים לתכנת. טכניקות נוספות לדיבאג כוללות פשוט כתיבת לוגים בקבצים, בדיקת תנאים ופונקציות שליטה רימוטיות על התוכנית שלנו. אם אתם מעוניינים לקרוא עוד על דיבאגינג בשפת ראסט, ניתן להתייעץ עם תיעוד הרישמי שלהם.

## ראו גם:

- התיעוד הרישמי של ראסט: https://www.rust-lang.org/he/documentation.html
- עמוד ה-Wiki של ראסט ב-Reddit: https://www.reddit.com/r/rust/wiki/faq#wiki_what_are_some_good_resources_for_learning_more.2Fgoing_deeper.3F
- כתבות ומדריכים מקוונים נוספים בנושא דיבאגינג בשפת ראסט.