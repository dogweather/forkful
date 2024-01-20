---
title:                "חישוב תאריך בעתיד או בעבר"
html_title:           "Go: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## מה זה? ולמה?

חישוב תאריך בעתיד או בעבר הוא פעולה של גיריסה של תאריך מסוים למרחק זמן קבוע. מתכנתים עושים את זה ללמשל כדי לנטר בעיתוי ולקבוע מועדים עתידיים.

## איך לעשות זאת:
```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	today := time.Now()
	tomorrow := today.AddDate(0, 0, 1)
	yesterday := today.AddDate(0, 0, -1)

	fmt.Printf("Today: %v\n", today.Format(time.RFC3339))
	fmt.Printf("Tomorrow: %v\n", tomorrow.Format(time.RFC3339))
	fmt.Printf("Yesterday: %v\n", yesterday.Format(time.RFC3339))
}
```
**הפלט**
```
Today: 2022-08-18T10:36:32+03:00
Tomorrow: 2022-08-19T10:36:32+03:00
Yesterday: 2022-08-17T10:36:32+03:00
```

## צלילה עמוקה 

חישובי התאריך המתוארים כאן מתאימים לא רק לחשבנה, אלא גם לתחישת מגבלות התאריך של מערכת הפעלה. לעיתים, המערכת יכולה להתמודד רק עם טווח מסוים של תאריכים. זוית נוספת היא חישוב התאריך בהתחשב בקפיצות. בחלק מהמדינות, מידי שנה השעה מתוקדמת או מואזרת בשעה אחת. אחריות המתכנת היא לזהות זמני הקפיצה ולהתאים את התאריך בהתאם.

## ראה גם:

- [המסמך הרשמי של שפת Go](https://golang.org/doc/)
- [ספריה זמינה של Go Time Package](https://golang.org/pkg/time/)
- [הידעת? - דילוגים בזמן](https://www.timeanddate.com/time/dst/)