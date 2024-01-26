---
title:                "ניתוח תאריך ממחרוזת"
date:                  2024-01-20T15:36:21.252403-07:00
html_title:           "Arduino: ניתוח תאריך ממחרוזת"
simple_title:         "ניתוח תאריך ממחרוזת"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
פענוח תאריך ממחרוזת זה התהליך שבו מתרגמים טקסט לתאריך שהמחשב יכול להבין. תכניתנים עושים זאת כי עליהם לעבוד עם תאריכים בפורמטים שונים, לשמור אותם במסדים ולהציג אותם למשתמשים.

## איך לעשות:
כאן נראה איך לעשות את זה ב-Gleam. דוגמא פשוטה:

```gleam
import gleam/calendar.{Date}
import gleam/should
import gleam/string.{from_slice}

fn parse_date(date_string: String) -> Result(Date, Nil) {
  // Your implementation here to parse the date string
  // לדוגמא, נניח שאנחנו מנתחים תאריך בפורמט YYYY-MM-DD
  "2023-03-14"
    |> from_slice
    |> String.split(separator: "-")
    |> should.map(split_string -> {
      case split_string {
        [year, month, day] ->
          Date.from_iso8601(year, month, day)
        _ ->
          Error(Nil)
      }
    })
}

fn main() {
  parse_date("2023-03-14")
}
```

תוצאת דוגמא:
```gleam
Ok(#Date(year: 2023, month: 3, day: 14))
```

## צלילה עמוקה
פענוח תאריכים הוא נושא שנלמד לעומק ביישומי קלנדר וזמנים. בעבר, תכניתנים נאבקו עם תאימות פורמטים ובעיות של אזורי זמן. כיום, ספריות כגון `gleam/calendar` מפשטות את המשימה. אלטרנטיבות כוללות פונקציונאליות מובנית במערכות בסיס נתונים או שימוש בספריות צד שלישי.

ביישום, יש להיות מודעים לתקן ISO 8601 עבור פורמט התאריך הבינלאומי ולזכור את החלוקה לאזורים זמנים. טיפול נכון בפורמט יכול למנוע בעיות תאימות עתידיות וטעויות של נתונים.

## ראה גם:
- [ISO 8601 Date and time format](https://www.iso.org/iso-8601-date-and-time-format.html)
- [Understanding Time Zones in Programming](https://www.w3.org/TR/timezone/)
