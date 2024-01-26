---
title:                "קבלת התאריך הנוכחי"
date:                  2024-01-20T15:14:34.589280-07:00
html_title:           "C: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?
לקבל את התאריך הנוכחי בקוד זה כמו לשאול את שעון העולם מה שעה - זה פשוט לומר למחשב "תגיד לי איזה יום אנחנו". תכנתים עושים את זה כדי לתעד אירועים, לחשב זמנים, או לתזמן משימות.

## איך לעשות:
בגלים עדכני, לקבל את התאריך והשעה הנוכחיים זה יחסית ישר לעניין. להלן דוגמא:

```gleam
import gleam/io
import gleam/os/time

pub fn main() {
  case time.now() {
    Ok(now) -> io.println("The current date and time is: " ++ now.to_string())
    Error(_) -> io.println("Unable to get the current date and time.")
  }
}
```

דוגמה לפלט:
```
The current date and time is: 2023-04-14T12:34:56Z
```

## צלילה לעומק
ההיסטוריה של קביעת זמן במחשבים היא מורכבת, אבל מה שחשוב לדעת זה שגלים משתמש בפונקציה `time.now()` כדי לקבל את הזמן המוחשי מהמערכת. יש אלטרנטיבות כמו ספריות צד שלישי ואפילו שימוש בפונקציות מערכת הפעלה ישירות, אבל הדרך שניתנה כאן היא הכי נוחה ו"גלימית". לגבי הפרטים הטכניים, `time.now()` מחזירה `Result(DateTime, TimeError)`, כלומר או תאריך ושעה או שגיאה, ומדוע זה חשוב? כי המשימה של להבטיח שתמיד יש לנו תאריך נוכחי לא תמיד פשוטה כמו שנראית.

## ראו גם
- המדריך הרשמי לשפת גלים: https://gleam.run/book/
- תיעוד Gleam לספריית הזמן: https://hexdocs.pm/gleam_stdlib/gleam/time/
- קורס קצר על עיבוד תאריכים וזמנים בפרוגרמינג: https://futurelearn.com/courses/programming-dates-and-times
