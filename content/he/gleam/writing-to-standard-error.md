---
title:                "כתיבה לפלט השגיאה הסטנדרטי"
date:                  2024-01-19
html_title:           "Arduino: כתיבה לפלט השגיאה הסטנדרטי"
simple_title:         "כתיבה לפלט השגיאה הסטנדרטי"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבה לסטנדרט ארור (standard error) משמשת לדיווח על שגיאות בזמן ריצת התוכנה. מתכנתים משתמשים בזה כדי להפריד הודעות שגיאה מפלט תקין.

## איך לעשות:
```gleam
import gleam/io

fn write_to_stderr() {
  let message = "שגיאה: הפעולה לא בוצעה כראוי"
  io.stderr_print(message)
}

pub fn main() {
  write_to_stderr()
}
```
פלט לדוגמא:
```
שגיאה: הפעולה לא בוצעה כראוי
```

## צלילה לעומק:
בעבר, תקני Unix חילקו את הפלט לשניים: סטנדרט אאוט (standard output) לפלט תקין וסטנדרט ארור לשגיאות. היתרון בהפרדה הוא שאפשר לנתבם ליעדים שונים, לדוגמה לקובץ או ישירות למסך. ב-Gleam, שימוש ב-io.stderr_print מאפשר כתיבה ישירה לסטנדרט ארור. זה שונה מ-io.print, שכותב לסטנדרט אאוט.

## ראה גם:
- [Unix standard streams article](https://en.wikipedia.org/wiki/Standard_streams)
- [Error handling in programming](https://en.wikipedia.org/wiki/Error_handling)
