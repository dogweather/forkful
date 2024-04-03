---
date: 2024-01-26 04:10:48.890213-07:00
description: "Rust \u05EA\u05D5\u05DE\u05DA \u05D1\u05DE\u05D2\u05D5\u05D5\u05DF \u05DE\
  \u05E0\u05E4\u05D9\u05DD, \u05D0\u05DA \u05DE\u05E0\u05E4\u05D4 \u05E0\u05E4\u05D5\
  \u05E5 \u05D0\u05D7\u05D3 \u05D4\u05D5\u05D0 `gdb` \u05E2\u05D1\u05D5\u05E8 GNU/Linux\
  \ \u05D0\u05D5 `lldb` \u05E2\u05D1\u05D5\u05E8 macOS. \u05EA\u05D5\u05DB\u05DC\u05D5\
  \ \u05D2\u05DD \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1`rust-gdb` \u05D0\u05D5\
  \ \u05D1`rust-lldb` \u05E9\u05D4\u05DD \u05DE\u05E2\u05D8\u05E4\u05D9\u05DD \u05E9\
  \u05DE\u05D3\u05E4\u05D9\u05E1\u05D9\u05DD\u2026"
lastmod: '2024-03-13T22:44:38.995179-06:00'
model: gpt-4-0125-preview
summary: "Rust \u05EA\u05D5\u05DE\u05DA \u05D1\u05DE\u05D2\u05D5\u05D5\u05DF \u05DE\
  \u05E0\u05E4\u05D9\u05DD, \u05D0\u05DA \u05DE\u05E0\u05E4\u05D4 \u05E0\u05E4\u05D5\
  \u05E5 \u05D0\u05D7\u05D3 \u05D4\u05D5\u05D0 `gdb` \u05E2\u05D1\u05D5\u05E8 GNU/Linux\
  \ \u05D0\u05D5 `lldb` \u05E2\u05D1\u05D5\u05E8 macOS."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05D3\u05D9\u05D1\u05D0\u05D2\u05E8"
weight: 35
---

## איך לעשות:
Rust תומך במגוון מנפים, אך מנפה נפוץ אחד הוא `gdb` עבור GNU/Linux או `lldb` עבור macOS. תוכלו גם להשתמש ב`rust-gdb` או ב`rust-lldb` שהם מעטפים שמדפיסים בצורה נאה ערכי Rust. הנה הצצה:

```Rust
fn main() {
    let mut counter = 0;
    for _ in 0..5 {
        counter += 1;
        println!("Counter is at: {}", counter);
    }
}
```

כדי לבצע איתור באגים בקוד זה, יש לקמפל עם מידע לאיתור באגים:

```shell
$ rustc -g counter.rs
```

אז להפעיל אותו ב`rust-gdb`:

```shell
$ rust-gdb counter
(gdb) break main
(gdb) run
(gdb) print counter
$1 = 0
(gdb) continue
Counter is at: 1
(gdb) print counter
$2 = 1
```

## צלילה עמוקה
איתור הבאגים קיים מאז *הימים הטובים* של כרטיסי חורים, והתפתחותו הייתה מתנה מהאלים. Rust מספקת את הכלים המשלה עם אינטגרציות ל-GDB ו-LLDB בשל טבעה הרמת-מערכת של השפה.

חלופות לאיתור באגים בקוד Rust כוללות שימוש בסביבות פיתוח משולבות (IDEs) עם המנפים המובנים שלהם, שחלק מוצאים אותם יותר אינטואיטיביים. כאלה פופולריים כוללים את CLion עם התוסף של Rust או Visual Studio Code עם ההרחבה של Rust.

לגבי היישום, Rust מייצרת סמלי איתור באגים שמנפים אלה מבינים, מה שחיוני לעבור דרך הקוד, להגדיר נקודות עצירה, ולבדוק משתנים בלי לאבד את השכל.

## ראו גם
- הספר על Rust בנושא איתור באגים: https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html#guidelines-for-error-handling
- Rust By Example בנושא שגיאות ואיתור באגים: https://doc.rust-lang.org/rust-by-example/error.html
- שרת שפת Rust (RLS) שמפעיל את הרחבת Rust של VS Code: https://github.com/rust-lang/rls
- איתור באגים ב-Rust עם Visual Studio Code: https://marketplace.visualstudio.com/items?itemName=rust-lang.rust
