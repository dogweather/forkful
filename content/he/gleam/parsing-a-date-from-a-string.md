---
title:                "פענוח תאריך ממחרוזת"
html_title:           "Bash: פענוח תאריך ממחרוזת"
simple_title:         "פענוח תאריך ממחרוזת"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?

ביצוע "פירסונג" לתאריך ממחרוזת הוא פעולה שבה אנו משנים את הצורה של מחרוזות לצורה של משתנה מסוג תאריך. מתכנתים עושים זאת כדי להפוך נתונים אקראיים להיות משמשים בקוד, כמו לבצע חישובים של משך זמן.

## איך לעשות את זה:

להלן דוגמה של קוד שלמ בשפת Gleam:

```gleam
import gleam/calendar.{iso8601, date}

fn main() {
  let date_string = "2022-03-09"
  let parsed_date = iso8601.date.parse(date_string)
  
  case parsed_date {
    Ok(date) -> date |> date.to_iso8601 |> io.println 
    Error(Nil) -> "Invalid date format" |> io.println
  }
}
```

הפלט של הקוד מעלה יהיה:
```
2022-03-09
```

## צלילה עמוקה:

מאז ימי Altair 8800, חלק מהיעדים הראשונים של מתכנתים היה למפר נתונים חוץ של תאריכים לצורות מוכרות יותר. בנוסף, ישנן טכניקות אלטרנטיביות לפירסונג של תאריך ממחרוזת, כמו שימוש בריקורוסיה ותבניות נוספות. בפרט, Gleam משתמש בספריית `gleam/calendar` כדי להפוך את הפונקציה `date.parse` פעולה פשוטה וקלה.

## המשך קריאה:

אם תרצה לעמוק יותר וללמוד יותר על Gleam ועל התהליך של parsing, אם תרצה להכיר דרך אחרת לעשות זאת, תוכל לבדוק את המקורות הבאים:

1. מדריך בסיסי לשפת Gleam: https://gleam.run/book/tour/
2. תיעוד של ספריית 'gleam/calendar': https://gleam.run/standard-libraries/library/gleam/calendar/0.2.4/
3. מקורות שהתייחסים ל-Parsing: https://www.cs.nmsu.edu/~rth/pretty-printers.html