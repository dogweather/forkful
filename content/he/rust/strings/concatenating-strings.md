---
date: 2024-01-20 17:35:41.153010-07:00
description: "\u05D4\u05D3\u05D1\u05E7\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA\
  \ \u05D4\u05D9\u05D0 \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05D0\u05E0\
  \u05D7\u05E0\u05D5 \u05DE\u05D7\u05D1\u05E8\u05D9\u05DD \u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05D5\u05EA \u05DC\u05D9\u05E6\u05D9\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05EA \u05D0\u05D7\u05EA \u05D2\u05D3\u05D5\u05DC\u05D4 \u05D9\u05D5\u05EA\
  \u05E8. \u05D0\u05E0\u05D7\u05E0\u05D5 \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA\
  \ \u05D6\u05D4 \u05DB\u05D0\u05E9\u05E8 \u05E8\u05D5\u05E6\u05D9\u05DD \u05DC\u05D1\
  \u05E0\u05D5\u05EA \u05DE\u05E9\u05E4\u05D8\u05D9\u05DD, \u05DE\u05E1\u05E8\u05D9\
  \u05DD, \u05D0\u05D5 \u05E7\u05D5\u05D3 \u05D3\u05D9\u05E0\u05DE\u05D9 \u05DE\u05D7\
  \u05DC\u05E7\u05D5\u05EA \u05D8\u05E7\u05E1\u05D8\u2026"
lastmod: '2024-02-25T18:49:37.213383-07:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05D3\u05D1\u05E7\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA\
  \ \u05D4\u05D9\u05D0 \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05D0\u05E0\
  \u05D7\u05E0\u05D5 \u05DE\u05D7\u05D1\u05E8\u05D9\u05DD \u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05D5\u05EA \u05DC\u05D9\u05E6\u05D9\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05EA \u05D0\u05D7\u05EA \u05D2\u05D3\u05D5\u05DC\u05D4 \u05D9\u05D5\u05EA\
  \u05E8. \u05D0\u05E0\u05D7\u05E0\u05D5 \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\u05EA\
  \ \u05D6\u05D4 \u05DB\u05D0\u05E9\u05E8 \u05E8\u05D5\u05E6\u05D9\u05DD \u05DC\u05D1\
  \u05E0\u05D5\u05EA \u05DE\u05E9\u05E4\u05D8\u05D9\u05DD, \u05DE\u05E1\u05E8\u05D9\
  \u05DD, \u05D0\u05D5 \u05E7\u05D5\u05D3 \u05D3\u05D9\u05E0\u05DE\u05D9 \u05DE\u05D7\
  \u05DC\u05E7\u05D5\u05EA \u05D8\u05E7\u05E1\u05D8\u2026"
title: "\u05E9\u05E8\u05E9\u05D5\u05E8 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
הדבקת מחרוזות היא תהליך שבו אנחנו מחברים מחרוזות ליצירת מחרוזת אחת גדולה יותר. אנחנו עושים את זה כאשר רוצים לבנות משפטים, מסרים, או קוד דינמי מחלקות טקסט שונות.

## איך לעשות:
```Rust
fn main() {
    let greeting = "שלום";
    let subject = "עולם";
    let exclamation = "!";

    // שיטה פשוטה עם האופרטור '+'
    let sentence = greeting.to_string() + " " + subject + exclamation;
    println!("{}", sentence); // פלט: שלום עולם!

    // שיטה נעימה יותר עם macro
    let sentence = format!("{} {}{}", greeting, subject, exclamation);
    println!("{}", sentence); // פלט: שלום עולם!
}
```

## צלילה לעומק:
בעבר, לפני שהפרוגרמינג או ראסט בכלל היו בתמונה, הדבקת מחרוזות בוצעה בשפות כמו C עם פונקציות כמו `strcat`. בראסט, שירהב את בטיחות הזיכרון, יש אפשרויות מובנות כדי למנוע טעויות נפוצות כמו גידול מחרוזת מעבר לזיכרון המוקצה לה.

השימוש באופרטור `+` להדבקת מחרוזות הוא פשוט אך לא אידיאלי מבחינת הביצועים כי הוא יוצר מחרוזת זמנית בכל הצמדה. בניגוד לזה, `format!` מאפשר משולבות מחרוזת מתקדמת יותר ויעילה יותר מבחינת הזיכרון והביצועים.

ראסט גם מציע את המבנה `String`, שמתוכנן לעבודה עם מחרוזות אשר יש לשנות אותן בזמן ריצה, ואת הסוג `&str`, שהוא ייצוג בלתי שינוי ומחולק של מחרוזת.

## ראה גם:
- [Rust Book: What is Ownership?](https://doc.rust-lang.org/book/ch04-01-what-is-ownership.html) - עקרונות של בעלות מחרוזת בראסט.
- [Rust By Example: Strings](https://doc.rust-lang.org/rust-by-example/std/str.html) - דוגמאות שימוש במחרוזות בראסט.
- [The Rust Standard Library Documentation: std::string::String](https://doc.rust-lang.org/std/string/struct.String.html) - מידע מפורט על המבנה `String` בראסט.
