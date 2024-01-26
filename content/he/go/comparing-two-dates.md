---
title:                "השוואת שתי תאריכים"
date:                  2024-01-20T17:33:24.757370-07:00
model:                 gpt-4-1106-preview
simple_title:         "השוואת שתי תאריכים"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מה ולמה?
להשוות שתי תאריכים זה בדיקה איזה מהם קדם או האם הם שווים. תכנתים עושים זאת ללוגיקה של תזמון, תפוגה או זמני משתמש.

## איך לעשות:
```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    date1 := time.Date(2023, time.March, 14, 0, 0, 0, 0, time.UTC)
    date2 := time.Date(2023, time.March, 15, 0, 0, 0, 0, time.UTC)

    if date1.Before(date2) {
        fmt.Println("תאריך 1 קודם לתאריך 2")
    } else if date1.After(date2) {
        fmt.Println("תאריך 1 אחרי תאריך 2")
    } else {
        fmt.Println("התאריכים זהים")
    }
}
```
פלט:
```
תאריך 1 קודם לתאריך 2
```

## צלילה עמוקה
ב-Golang ההשוואה בין תאריכים נעשית בעזרת החבילה `time`. לפני Go, השוואת תאריכים הייתה תלוית מערכת וקשה יותר לביצוע. החלופות כוללות ספריות חיצוניות כגון `Date` ב-JavaScript. ב-Go, פונקציות כגון `Before`, `After` ו-`Equal` הופכות את זה לפשוט ונקי.

## גם ראה
- מסמך רשמי של חבילת הזמן ב-Go: https://golang.org/pkg/time/
- הדרכה לעבודה עם תאריכים וזמנים ב-Go: https://gobyexample.com/time
- מאמר על ניתוח תאריכים ב-Go: https://yourbasic.org/golang/time-change-date-format-parse-generate/
