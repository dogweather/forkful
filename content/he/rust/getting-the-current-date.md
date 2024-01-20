---
title:                "קבלת התאריך הנוכחי"
html_title:           "C#: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?

לקבלת התאריך הנוכחי בצורה דינאמית זו פעולה בסיסית בתכנות שמקנה לאפליקציה שלנו התאמה לזמן בו היא פעילה. זה משמיע לנו כמה אנחנו מתקדמים - בדקות, שעות, ימים, שנים.

## איך ל:
```
use chrono::{Date, Local};

fn main() {
    let local: Date<Local> = Local::today();
    println!("{}", local.format("%d/%m/%Y").to_string());
}
```
תצאה:
```
20/03/2023
```
השימוש ב-`chrono` מאפשר לנו לקבלת את התאריך הנוכחי בקלות, ואז לעצב אותו לפי הפורמט שנבחר.

## צלילה עמוקה
החבילה `chrono` היא לא הארגז היחיד שרוד בשוק. לפני שהייתה חבילת 'chrono', תכנותים השתמשו ב-'time` ומספר ארגזים אחרים. ארגז ה-'chrono' מאפשר יצירת זמן מקומי ומאפשר פורמט אישי.

## ראה גם
- [תיעוד חבילת chrono](https://docs.rs/chrono/0.4.19/chrono/)
- [המדריך לrust](https://doc.rust-lang.org/stable/book/)
- [Stack Overflow: כיצד לקבל את התאריך הנוכחי ב-Rust](https://stackoverflow.com/questions/27581523/how-to-get-the-current-date)
- [Chanakya: כיצד להמיר את התאריך והשעה הנוכחיים למחרוזת ב-RUST](https://chanakya.medium.com/how-to-convert-the-current-date-and-time-to-string-in-rust-8fda0f630e7b)