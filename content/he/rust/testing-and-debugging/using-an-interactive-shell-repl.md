---
date: 2024-01-26 04:18:55.572082-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA \u05D6\u05D0\u05EA\
  : \u05DB\u05E8\u05D2\u05E2, \u05DC-Rust \u05D0\u05D9\u05DF REPL \u05E8\u05E9\u05DE\
  \u05D9 \u05E9\u05DE\u05D2\u05D9\u05E2 \u05E2\u05DD \u05D4\u05E9\u05E4\u05D4. \u05D0\
  \u05EA\u05D4 \u05D9\u05DB\u05D5\u05DC \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\
  \u05DB\u05DC\u05D9\u05DD \u05E9\u05DC \u05E6\u05D3 \u05E9\u05DC\u05D9\u05E9\u05D9\
  \ \u05DB\u05DE\u05D5 `evcxr_repl`. \u05D4\u05EA\u05E7\u05DF \u05D0\u05D5\u05EA\u05D5\
  \ \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA Cargo."
lastmod: '2024-03-13T22:44:38.990217-06:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05E8\u05D2\u05E2, \u05DC-Rust \u05D0\u05D9\u05DF REPL \u05E8\u05E9\
  \u05DE\u05D9 \u05E9\u05DE\u05D2\u05D9\u05E2 \u05E2\u05DD \u05D4\u05E9\u05E4\u05D4\
  ."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E2\u05D8\u05E4\u05EA \u05D0\
  \u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05D8\u05D9\u05D1\u05D9\u05EA (REPL)"
weight: 34
---

## איך לעשות זאת:
כרגע, ל-Rust אין REPL רשמי שמגיע עם השפה. אתה יכול להשתמש בכלים של צד שלישי כמו `evcxr_repl`. התקן אותו באמצעות Cargo:

```sh
cargo install evcxr_repl
```

לאחר מכן, הפעל את ה-REPL:

```sh
evcxr
```

בתוכו, נסה קצת קוד Rust:

```rust
let x = 5;
let y = 3;
println!("{} + {} = {}", x, y, x + y);
```

הפלט אמור להיות:

```
5 + 3 = 8
```

## צלילה עמוקה
האתוס של Rust ממוקד סביב בטיחות וביצועים, אשר בדרך כלל משויכים לשפות שמקומפלות מראש, פחות עם שפות מפורשות, ידידותיות ל-REPL. באופן היסטורי, שפות כמו Python או Ruby שמו דגש על בעלות REPL למשוב מיידי, אך לא תוכננו עם מטרות ברמת המערכת בראש.

למרות חוסרה של REPL רשמית ב-Rust, כמה אלטרנטיבות כמו `evcxr_repl` צמחו. הפרויקטים הללו אינם רק "משחקים" עם Rust כדי להפוך אותה ל-REPL; הם קוראים בחכמה את מעגל הקומפילציה והריצה של השפה לסשן אינטרקטיבי. ה-REPL מקומפל את הקוד ברקע ומריץ את הבינארי, תוך תיעוד הפלט. בדרך זו, היא שומרת על יתרונות הביצועים של Rust תוך כדי שהיא מעניקה את חווית האינטראקציה.

יש דיון מתמשך בקהילת Rust לגבי תמיכה רשמית ב-REPL, ועם כל איטרציה של השפה, אנו רואים רמת מסופקות כלים שעשויה להוביל בסופו של דבר לפתרון מובנה.

## ראה גם
למידע נוסף וכלים אחרים:
- מאגר GitHub של Evcxr REPL: [https://github.com/google/evcxr](https://github.com/google/evcxr)
- Rust Playground, דרך מקוונת לבדוק קוד Rust: [https://play.rust-lang.org/](https://play.rust-lang.org/)
- דיון בשפת Rust על תכונת REPL: [https://internals.rust-lang.org/](https://internals.rust-lang.org/)
