---
date: 2024-01-26 03:44:08.875442-07:00
description: "\u05D4\u05E1\u05E8\u05EA \u05E6\u05D9\u05D8\u05D5\u05D8\u05D9\u05DD\
  \ \u05DE\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D1-Rust \u05D4\u05D9\u05D0 \u05E2\
  \u05E0\u05D9\u05D9\u05DF \u05E9\u05DC \u05DC\u05E4\u05E9\u05D5\u05D8 \u05EA\u05D5\
  \u05D5\u05D9 \u05E6\u05D9\u05D8\u05D5\u05D8 \u05DE\u05D9\u05D5\u05EA\u05E8\u05D9\
  \u05DD \u05E9\u05D9\u05DB\u05D5\u05DC\u05D9\u05DD \u05DC\u05D4\u05D9\u05DB\u05DC\
  \u05DC \u05E1\u05D1\u05D9\u05D1 \u05D4\u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D4\
  \u05D8\u05E7\u05E1\u05D8\u05D5\u05D0\u05DC\u05D9\u05D9\u05DD \u05E9\u05DC\u05DA\
  . \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\
  \u05EA \u05DB\u05D0\u05E9\u05E8 \u05D4\u05DD \u05E6\u05E8\u05D9\u05DB\u05D9\u05DD\
  \ \u05DC\u05E0\u05E7\u05D5\u05EA \u05D0\u05D5\u2026"
lastmod: '2024-02-25T18:49:37.207105-07:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05E1\u05E8\u05EA \u05E6\u05D9\u05D8\u05D5\u05D8\u05D9\u05DD \u05DE\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D1-Rust \u05D4\u05D9\u05D0 \u05E2\u05E0\
  \u05D9\u05D9\u05DF \u05E9\u05DC \u05DC\u05E4\u05E9\u05D5\u05D8 \u05EA\u05D5\u05D5\
  \u05D9 \u05E6\u05D9\u05D8\u05D5\u05D8 \u05DE\u05D9\u05D5\u05EA\u05E8\u05D9\u05DD\
  \ \u05E9\u05D9\u05DB\u05D5\u05DC\u05D9\u05DD \u05DC\u05D4\u05D9\u05DB\u05DC\u05DC\
  \ \u05E1\u05D1\u05D9\u05D1 \u05D4\u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05D4\u05D8\
  \u05E7\u05E1\u05D8\u05D5\u05D0\u05DC\u05D9\u05D9\u05DD \u05E9\u05DC\u05DA. \u05EA\
  \u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA\
  \ \u05DB\u05D0\u05E9\u05E8 \u05D4\u05DD \u05E6\u05E8\u05D9\u05DB\u05D9\u05DD \u05DC\
  \u05E0\u05E7\u05D5\u05EA \u05D0\u05D5\u2026"
title: "\u05D4\u05E1\u05E8\u05EA \u05DE\u05E8\u05DB\u05D0\u05D5\u05EA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?

הסרת ציטוטים ממחרוזת ב-Rust היא עניין של לפשוט תווי ציטוט מיותרים שיכולים להיכלל סביב הנתונים הטקסטואליים שלך. תכנתים עושים זאת כאשר הם צריכים לנקות או לנרמל מחרוזות, אולי לאחר ניתוח נתונים מקובץ, או כשהם מכינים אותה לפורמט אחר שבו ציטוטים עשויים להיות בעייתיים או מיותרים.

## איך לעשות:

```Rust
fn remove_quotes(s: &str) -> String {
    s.trim_matches(|c| c == '\"' || c == '\'').to_string()
}

fn main() {
    let quoted_str = "\"שלום, רוסטצ'יאנים!\"";
    let cleaned_str = remove_quotes(quoted_str);
    println!("{}", cleaned_str);
    // Output: שלום, רוסטצ'יאנים!
}
```

לפעמים יש לך מחרוזת עם ציטוטים מעורבים, כמו זו:

```Rust
fn main() {
    let mixed_quoted = "'רוסט אומרת: \"שלום, עולם!\"'";
    let cleaned_str = remove_quotes(mixed_quoted);
    println!("{}", cleaned_str);
    // Output: רוסט אומרת: "שלום, עולם!"
}
```

כאן, רק הציטוטים החיצוניים ביותר מוסרים.

## צלילה עמוקה

כאשר מסירים ציטוטים ממחרוזת, ייתכן ותתהה למה זה לא פשוט `.replace("\"", "")`. בתחילה, העיסוק בטקסט היה פחות מתוקנן, ומערכות שונות היו להן דרכים שונות לאחסון ולהעברת טקסט, לעיתים קרובות עם סוג של 'רצף בריחה' עבור תווים מיוחדים. המתודה `trim_matches` של Rust גמישה יותר, מאפשרת לך לציין תווים מרובים לקיצוץ, ואם לקצץ מהתחלה (קידומת), מהסוף (סיומת), או משני צידי המחרוזת.

כמובן, ישנן חלופות. Regex הוא כוח עצום למניפולציה על מחרוזות, מסוגל להתאים לתבניות מורכבות, ויהיה קילוף עודף רק להסרת ציטוטים. ספריות כמו `trim_in_place` יכולות להציע קיצוץ במקום ללא העלויות של יצירת אובייקט `String` חדש, שיכול להיות רצוי ליישומים קריטיים בביצועים.

מאחורי הקלעים, `trim_matches` למעשה עובר דרך התווים של המחרוזת משני הקצוות, בודק מול התבנית המסופקת עד שנמצא תו שאינו תואם. זה יעיל למה שהוא עושה, אבל תמיד שים לב שהוא עובד עם ערכי סקלר של יוניקוד. אם המחרוזת שלך עשויה לכלול תווים יוניקוד מרובי-בתים, אין צורך לדאוג שיפריד אותם.

## ראה גם

- התיעוד של Rust על מניפולציה על מחרוזות: https://doc.rust-lang.org/book/ch08-02-strings.html
- ה-crate `regex` עבור תבניות מורכבות: https://crates.io/crates/regex
- Rust לפי דוגמא לסצנריות קוד מעשיות: https://doc.rust-lang.org/stable/rust-by-example/std/str.html
