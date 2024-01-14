---
title:    "Rust: יצירת מספרים אקראיים"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## למה

כדי להפעיל אפליקציות שמשתמשות במספרים אקראיים, כמו משחקי מחשב או יצירת פאזלים משובצים.

## איך לעשות זאת

הקלדה של המרת מספרים אקראיים בשפת Rust היא פשוטה ויעילה. למטה תוכלו למצוא דוגמאות לקוד ולפלט תוצרתי.

```Rust
// ייבוא חבילת rand המאפשרת ליצור מספרים אקראיים
use rand::Rng;

fn main() {
    // יצירת מספר אקראי בין 1 ל-100
    let random_num = rand::thread_rng().gen_range(1, 101);
    println!("המספר האקראי הוא: {}", random_num);
}
```

פלט תוצרתי:

```
המספר האקראי הוא: 57
```

## טביעת רגל לעומק

יצירת מספרים אקראיים בשפת Rust מתבצעת על ידי שימוש בחבילת rand המאפשרת לנו ליצור טווחים של מספרים אקראיים ולהחזיר תוצאות בצורה יעילה ומהירה. ניתן גם להשתמש בפונקציות מתקדמות של החבילה כדי ליצור מספרים אקראיים עם תכונות מסוימות, כגון כתובת IP או ביטוי RegEx. כמו כן, ניתן לשלב את יכולות החבילה עם נתונים מסוימים כדי ליצור רעיונות בתצורת מספרים אקראיים חדשים.

## ראו גם

- תיעוד על rand באתר GitHub: https://github.com/rust-random/rand
- היסטוריית שנוי אקראיים: https://en.wikipedia.org/wiki/Randomness 
- מדריך מפורט על השתמשות בחבילת rand בשפת Rust: https://doc.rust-lang.org/book/ch07-05-separating-modules-into-different-files.html