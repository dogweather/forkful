---
title:                "המרת תאריך למחרוזת"
date:                  2024-01-20T17:36:43.436326-07:00
model:                 gpt-4-1106-preview
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
המרת תאריך למחרוזת היא פעולה שבה אתה משנה את פורמט התאריך מאובייקט תאריך לטקסט. תוכניתנים עושים זאת כדי להציג את התאריך באופן ידידותי למשתמש או לשמור תאריכים בבסיס נתונים.

## איך לעשות:
```gleam
import gleam/calendar
import gleam/io

fn demo() {
  let date = calendar.date(2023, 4, 12)
  let string_date = date |> calendar.to_string
  io.print(string_date)
}

pub fn main() {
  demo()
}
```

פלט לדוגמה:
```
"2023-04-12"
```

## צלילה עמוקה
המרת תאריך למחרוזת אינה חדשה - זו תרגיל נפוץ בכל תחומי התכנות. ב-Gleam, מודול ה-calendar מספק פונקציונליות לעבודה עם תאריכים. ישנן שיטות אחרות להמרה, כמו ביבליות חיצוניות או תכנות ידני של פונקציית המרה. היישום ב-Gleam מראה שהשפה מקדמת עבודה עם טיפוסים ידודיים לצד כלי סטנדרטי שמפשטים את המשימה.
