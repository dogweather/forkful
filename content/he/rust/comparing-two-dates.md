---
title:                "Rust: השוואת שתי תאריכים"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## למה

קישור חזק בין שתי תאריכים הוא חיוני עבור תכנות פונקציות על תאריכים מסוימים. השוואה בין שני תאריכים עלולה להיות אתגר מתמיד, והשתמשות בכלי יעיל כמו Rust עשויה לפתור בעיות מאתגרות זאת.

## כיצד לעשות זאת

עבור לאורך דוגמאות קוד שלך ופלט תוצאות תוך שימוש בבלוקי קוד "```Rust ... ```". קוד זה יראה כך:

```Rust
fn main() {
    let date1 = "2021-04-05";
    let date2 = "2019-12-15";
    let result = date1 > date2;
    println!("The result of comparing date1 and date2 is: {}", result);
}
```

מגיע עם הפלט הבא:

```
The result of comparing date1 and date2 is: true
```

בקוד למעלה, אנו משווים שני תאריכים ומדפיסים את התוצאה. בכך אנו מראים את יכולת Rust להתמודד עם שני תאריכים תחת מבנה תאים ביעילות.

## חקירה עמוקה

השוואה בין שני תאריכים בעזרת Rust מתבצעת על ידי האצה והמרה של התאריך למספר שלם ואז השוואה של המספרים. המספרים ימי השנה היחודיים שעברו מתאריך ההתחלה של Unix, שהוא 1 בינואר 1970. ככל שמספר הימים היחודיים גדל, כך גדלת תאריך השנה הממוממשת, ובכך גם הניסוח הייחודי להשוואה של תאריכים בשפת Rust.

## ראה גם

- [רשמי Rust עמותה](https://rust-lang.org)
- [חוברת המדריך עבור לאמן שלתוכניות Rust מסובבות תאריכים](https://doc.rust-lang.org/rust-by-example/primitives/date.html)
- [הוראות לקוד Rust עבור השוואת תאריכים ממשהו שלך](https://docs.rs/chrono/0.4.19/chrono/format/strftime/index.html)