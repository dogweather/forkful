---
title:    "Rust: השוואת שתי תאריכים"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## למה

כתבה זו תסביר לך למה חשוב לשווא את שתי תאריכים בתוכנות ראסט.

## איך לעשות זאת

באמצעות דוגמאות קוד ופלט דוגמא בתוך בלוקי קוד ```Rust...```, תוכלו ללמוד כיצד לשווא שני תאריכים באמצעות שפת התכנות ראסט.

```Rust
use chrono::{NaiveDate, Duration}; // import packages

// create two sample dates
let date1 = NaiveDate::from_ymd(2021, 10, 1);
let date2 = NaiveDate::from_ymd(2020, 9, 15);

// compare using the PartialOrd trait
if date1 < date2 {
    println!("{} is before {}", date1, date2)
} else if date1 > date2 {
    println!("{} is after {}", date1, date2)
} else {
    println!("{} is equal to {}", date1, date2)
}
```

הפלט שיוצא הוא:

```bash
2021-10-01 is after 2020-09-15
```

## חפירה עמוקה

לפני שכותבים קוד לשווא שני תאריכים, חשוב להבין מה מטרת השווא וכיצד זה ישפיע על עבודת התוכנה שלכם. ניתן לשווא שני תאריכים למטרות שונות, כגון יצירת תאריך חדש, בדיקת תקינות תאריך, או ניהול תאריכים באופן כללי.

לכל מטרה ישנם פתרונות שונים והשווא שני תאריכים בעזרת ראסט הוא רק אחד מהם. כדי להיות מקצועיים יותר בשימוש בשפת תכנות זו, עלינו ללמוד כיצד להשתמש בכלים נוספים לניהול ושינוי תאריכים באופן יעיל ויעיל.

## ראה גם

- [מדריך עבודה עם תאריכים בראסט](https://dev.to/mschwarzmueller/the-complete-rust-lang-guide-for-newcomers-and-enthusiasts-part-5-dates-datetime-rustb50)
- [מדריך בסיסי לטיפול בתאריכים בראסט](https://blog.logrocket.com/dates-and-time-in-rust/)
- [תיעוד רשמי על פעולות תאריכים בראסט](https://doc.rust-lang.org/std/time/index.html)