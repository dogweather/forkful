---
title:                "פענוח תאריך ממחרוזת"
html_title:           "Bash: פענוח תאריך ממחרוזת"
simple_title:         "פענוח תאריך ממחרוזת"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?

הרצה של תאריך ממחרוזת היא התהליך שבו מתכנת מאחזר תאריך מתוך מחרוזת. זהו כלי שימושי עבור מתכנתים בעת עיבוד ואחזור נתונים הנמצאים בצורה של מחרוזות, כמו נתונים שנשלחים באמצעות HTTP או נשמרים במסד נתונים.

## איך מבצעים? 

נסקור דוגמה של קוד Go שהוא מפענח תאריך ממחרוזת:

```Go
package main

import (
  "fmt"
  "time"
)

func main() {
  var myDateString = "2022-02-22T15:04:05Z[07:00]"
  myTime, _ := time.Parse(time.RFC3339, myDateString)
  fmt.Println("Parsed time:", myTime)
}
```

בתצוגת הפלט, תקבלו תאריך שהתקבל מהמחרוזת:

```
Parsed time: 2022-02-22 15:04:05 +0700 +0700
```

## שיעור מעמיק 

עם היסטוריה של שימוש בשפות אחרות על שבילים דומים, Go מאפשר למתכנתים להפעיל पריסה של תאריך ממחרוזת ביעילות. 
Go מציעה שערים חלופיים לכך, כולל `ParseInLocation`, אשר מאפשרת התאמה לאזור זמן מסויים. 
פרטי הביצוע אינם קשה להגיע, כאשר הקוד מסתמך על הספרייה הסטנדרטית `time`.

## ראה גם

עיין במקורות הבאים למידע נוסף ומתקדם יותר:

1. [מסמך רשמי של Go לגבי Time](https://golang.org/pkg/time/)
3. [מדריך מקוצר ל-Go Date & Time Parsing](https://yourbasic.org/golang/format-parse-string-time-date-example/)