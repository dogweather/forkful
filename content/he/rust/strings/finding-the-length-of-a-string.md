---
date: 2024-01-20 17:48:41.581831-07:00
description: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05E9\u05DC\
  \ \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D1-Rust \u05D6\u05D4\u05D5 \u05E4\u05E2\
  \u05D5\u05DC\u05D4 \u05E9\u05D1\u05D4 \u05D0\u05E0\u05D7\u05E0\u05D5 \u05DE\u05D3\
  \u05D3\u05D9\u05DD \u05D0\u05EA \u05DE\u05E1\u05E4\u05E8 \u05D4\u05EA\u05D5\u05D5\
  \u05D9\u05DD \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA. \u05D6\u05D4 \u05E0\u05E2\
  \u05E9\u05D4 \u05DC\u05E7\u05D1\u05D9\u05E2\u05EA \u05DE\u05D9\u05D3\u05D5\u05EA\
  , \u05DC\u05D5\u05DC\u05D0\u05D5\u05EA \u05D5\u05D1\u05D3\u05D9\u05E7\u05D5\u05EA\
  \ \u05EA\u05E7\u05E4\u05D5\u05EA \u05D1\u05E7\u05D5\u05D3."
lastmod: '2024-03-13T22:44:38.972471-06:00'
model: gpt-4-1106-preview
summary: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05E9\u05DC \u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05D1-Rust \u05D6\u05D4\u05D5 \u05E4\u05E2\u05D5\
  \u05DC\u05D4 \u05E9\u05D1\u05D4 \u05D0\u05E0\u05D7\u05E0\u05D5 \u05DE\u05D3\u05D3\
  \u05D9\u05DD \u05D0\u05EA \u05DE\u05E1\u05E4\u05E8 \u05D4\u05EA\u05D5\u05D5\u05D9\
  \u05DD \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA."
title: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA"
weight: 7
---

## כיצד לעשות:
כדי למצוא את אורך המחרוזת ב-Rust, אנחנו משתמשים במתודה `.len()`. זה פשוט ככה:

```rust
fn main() {
    let greeting = "שלום";
    println!("אורך המחרוזת הוא: {}", greeting.len());
}
```

תוצאה:

```
אורך המחרוזת הוא: 8
```

שימו לב, הערך שחוזר מהמתודה `.len()` הוא מספר הבתים, לא התווים. במחרוזת המעוגלת "שלום", יש ארבעה תווי UTF-8, אבל כל אחד מהם תופס שני בתים, לכן האורך 8.

## עיון מעמיק
אורך מחרוזת ב-Rust קשור למושג של בתים, לא תווים. בעבר, שפות כמו C השתמשו במונה עבור כל תו, אבל זה לא עובד טוב עם תווים שלוקחים יותר מבית אחד, כמו UTF-8.

אלטרנטיבות למתודה `.len()` כוללות בחינת כל תו בלולאה וחישוב של אורך על פי קוד נקודה יחיד עבור כל תו, או שימוש במתודות של ספריית התקנית לעבודה ישירה עם תווי Unicode.

הפיתרון לעבודה עם תווים במקום בתים הוא להשתמש במתודות מסוג `.chars().count()`:

```rust
fn main() {
    let greeting = "שלום";
    let char_count = greeting.chars().count();
    println!("מספר התווים במחרוזת הוא: {}", char_count);
}
```

תוצאה:

```
מספר התווים במחרוזת הוא: 4
```

המתודה `.chars()` יוצרת איטרטור של תווים שממנו אפשר לעשות `.count()` כדי למצוא את המספר האמיתי של תווים Unicode במחרוזת.

## ראה גם
- תיעוד השפה על מחרוזות: https://doc.rust-lang.org/std/string/
- למידה נוספת על Unicode ב-Rust: https://doc.rust-lang.org/book/ch08-02-strings.html
- פורום הקהילה של Rust, שם תוכלו לשאול שאלות: https://users.rust-lang.org/
