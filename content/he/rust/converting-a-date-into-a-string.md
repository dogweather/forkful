---
title:                "Rust: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

תוכן:

## למה

מדוע אנחנו צריכים להמיר תאריך למחרוזת? מה ההימצאות של מחרוזת תאריך ומדוע זה כה חשוב בתוכניות רבות? במאמר הזה נדבר על הטכניקות השונות להמרת תאריך למחרוזת בשפת ראסט ולמה זה כל כך חשוב.

## כיצד לעשות זאת

ראסט מציע שיטות רבות להמרת תאריך למחרוזת ואנחנו נראה כמה מהן במאמר הזה.

```Rust
use chrono::{DateTime, Utc, format::{Utc, ChronoError, format, strftime}};
 
fn main() {
    // יצירת אובייקט תאריך חדש בפורמט UTC
    let date = Utc::now();
    
    // המרת התאריך למחרוזת בפורמט מותאם אישית
    let formatted_date = date.format("%d/%m/%Y").to_string();
    
    println!("{}", formatted_date); // פלט: 28/08/2021
}
```

בדוגמה הזאת אנחנו משתמשים בספריית chrono ליצירת אובייקט תאריך ולהמרתו למחרוזת באמצעות פונקציות כמו format ו strftime. אפשר להשתמש גם בספריית time לצורך תאריך ושעה עם פונקציות דומות.

## צלילה עמוקה

המרת תאריך למחרוזת היא תוקף בתוכניות רבות ולכן חיוני לדעת כיצד לעשות זאת בצורה נכונה ויעילה. במאמר הזה ראינו רק חלק קטן מהתוכן העמוק שיש בהמרת תאריך למחרוזת בשפת ראסט, אך אנחנו מבטיחים שאם תמסור עלינו, תכיר את כל הטכניקות החשובות לעשות זאת כהדרגה.

## ראה גם

- דוגמאות נוספות להמרת תאריך למחרוזת בשפת ראסט: [https://www.snoyman.com/blog/2018/12/rust-date-and-time-parsing](https://www.snoyman.com/blog/2018/12/rust-date-and-time-parsing)
- תיעוד רשמי על המרת תאריך למחרוזת בספריית chrono: [https