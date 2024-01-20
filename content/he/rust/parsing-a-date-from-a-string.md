---
title:                "פענוח תאריך ממחרוזת"
html_title:           "Bash: פענוח תאריך ממחרוזת"
simple_title:         "פענוח תאריך ממחרוזת"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# פענוח תאריך מתוך מחרוזת בשפת התכנות Rust

## מה זה ולמה? 
פענוח תאריך מתוך מחרוזת הוא התהליך שבו אנחנו מאפשרים לתוכנה לזהות ולהבין תאריך המתואר בצורת מלל. זה חשוב כדי שנוכל לבצע פעולות מבוססות-תאריך, כמו כפל וחישובי זמן.

## איך ל - :
נוכל לעשות את זה באמצעות החבילה הסטנדרטית `chrono` של Rust. הנה דוגמה של קוד:

```Rust
extern crate chrono;
use chrono::datetime::DateTime;
use chrono::format::ParseError;

fn parse_date(date_str: &str) -> Result<DateTime<Utc>, ParseError> {
    DateTime::parse_from_rfc3339(date_str)
}

fn main() {
    let date_str = "2022-10-18T07:30:00Z";
    let date = parse_date(date_str).unwrap();
    println!("{:#?}", date);
}
```

שירות הפלט של קוד זה:
``` 
ש",
2022-10-18T07:30:00Z
```

## בהעמק:
לשפת Rust לא היתה תמיכה מובנית לטיפול בתאריכים ושעות למשך הרבה שנים. בתחילה, המתכנתים השתמשו
בדינמיקליב C להמיר מחרוזת לתאריך. Chrono הוא אחד מהספריות המובילה בRust לתאריכים ושעות, הוכחשה על ידי 
אדף ממונייה מאותה שפה.
מעבר לזה, ניכנו במחרוזת הן שיטות מעולות לקשיים בעייתיים. לדוגמה, בשפות אחרות מן PaserError 
אם המחרוזת היא לא חוקית, או אולי DateTime::parse_from_rfc3339 של Chrono תחזיר תאריך השסהות 
יש לראות אותו בזמן ממשי, אם הפוך.

## ראה גם:
* [מסמך ראשי של Chrono](https://docs.rs/chrono/0.4.19/chrono/)