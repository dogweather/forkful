---
date: 2024-01-26 04:10:48.890213-07:00
description: "\u05D4\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E0\u05E4\u05D4\
  \ \u05D4\u05D5\u05D0 \u05DB\u05DE\u05D5 \u05DC\u05EA\u05EA \u05DC\u05E2\u05E6\u05DE\
  \u05DA \u05E8\u05D0\u05D9\u05D9\u05EA X \u05DC\u05D4\u05E6\u05D9\u05E5 \u05DC\u05EA\
  \u05D5\u05DA \u05D1\u05D9\u05E6\u05D5\u05E2 \u05D4\u05E7\u05D5\u05D3 \u05E9\u05DC\
  \u05DA. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\
  \u05D0\u05EA \u05D1\u05DB\u05D3\u05D9 \u05DC\u05D0\u05EA\u05E8 \u05EA\u05E7\u05DC\
  \u05D5\u05EA, \u05DC\u05D4\u05D1\u05D9\u05DF \u05D0\u05EA \u05D6\u05E8\u05D9\u05DE\
  \u05EA \u05D4\u05EA\u05DB\u05E0\u05D9\u05EA, \u05D5\u05DC\u05D5\u05D5\u05D3\u05D0\
  \ \u05E9\u05D4\u05E7\u05D5\u05D3 \u05E9\u05DC\u05D4\u05DD \u05E0\u05E7\u05D9 \u05DB\
  \u05DE\u05E9\u05E8\u05D5\u05E7\u05D9\u05EA.\u2026"
lastmod: 2024-02-19 22:04:58.207286
model: gpt-4-0125-preview
summary: "\u05D4\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E0\u05E4\u05D4 \u05D4\
  \u05D5\u05D0 \u05DB\u05DE\u05D5 \u05DC\u05EA\u05EA \u05DC\u05E2\u05E6\u05DE\u05DA\
  \ \u05E8\u05D0\u05D9\u05D9\u05EA X \u05DC\u05D4\u05E6\u05D9\u05E5 \u05DC\u05EA\u05D5\
  \u05DA \u05D1\u05D9\u05E6\u05D5\u05E2 \u05D4\u05E7\u05D5\u05D3 \u05E9\u05DC\u05DA\
  . \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\
  \u05EA \u05D1\u05DB\u05D3\u05D9 \u05DC\u05D0\u05EA\u05E8 \u05EA\u05E7\u05DC\u05D5\
  \u05EA, \u05DC\u05D4\u05D1\u05D9\u05DF \u05D0\u05EA \u05D6\u05E8\u05D9\u05DE\u05EA\
  \ \u05D4\u05EA\u05DB\u05E0\u05D9\u05EA, \u05D5\u05DC\u05D5\u05D5\u05D3\u05D0 \u05E9\
  \u05D4\u05E7\u05D5\u05D3 \u05E9\u05DC\u05D4\u05DD \u05E0\u05E7\u05D9 \u05DB\u05DE\
  \u05E9\u05E8\u05D5\u05E7\u05D9\u05EA.\u2026"
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05D3\u05D9\u05D1\u05D0\u05D2\u05E8"
---

{{< edit_this_page >}}

## מה ולמה?

השימוש במנפה הוא כמו לתת לעצמך ראיית X להציץ לתוך ביצוע הקוד שלך. תכנתים עושים זאת בכדי לאתר תקלות, להבין את זרימת התכנית, ולוודא שהקוד שלהם נקי כמשרוקית. זה כמו להוסיף חבר שמצביע בדיוק איפה נפלתם.

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
