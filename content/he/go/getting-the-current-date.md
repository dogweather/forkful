---
title:                "קבלת התאריך הנוכחי"
html_title:           "C#: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

# מה ולמה?

השגת התאריך הנוכחי היא גלישה לשרת השעון של המחשב והבאת הערך. זה נדרש למגוון רחב של משימות, כגון יצירת תוויות זמן בלוגים, ניהול עסקאות באופן נכון, ועוד.

# איך לעשות:

ניתן לקבל את התאריך והשעה הנוכחיים ב-Golang באמצעות הפונקציה time.Now().

```Go
package main

import "fmt"
import "time"

func main() {
    fmt.Println(time.Now())
}
```

בריצת הקוד, יודפס השעה והתאריך הנוכחיים.

# צלילה עמוקה

1. קשר היסטורי: מאז תחילת התכנות, הייתה צורך בשיטה לאחזור התאריך והשעה הנוכחיים. למרות העדכונים והשיפורים לאורך השנים, הקונספט המרכזי נשאר זהה.

2. אלטרנטיבות: ישנה תמיכה רחבה בשפות תכנות אחרות עם פונקציות, כמו python's datetime.now() או JavaScript's Date().

3. פרטי יישום: ב-Golang, הפונקציה time.Now() מחזירה ערך מסוג Time המייצג את התאריך והשעה בזמן ההפעלה.

# ראה גם:

לינקים למקורות מקוונים נוספים על הנושא:

1. [מסמכי ה-API של Golang לגבי Time](https://golang.org/pkg/time/#Time)
2. [Tutorial Golang שלנו על שימוש בספרייה של Time](https://www.tutorialspoint.com/go/go_date_time.htm)
3. [שאלות נפוצות על נושא ב-StackOverflow](https://stackoverflow.com/questions/tagged/go+time)