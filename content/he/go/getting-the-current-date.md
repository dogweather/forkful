---
title:                "לקבלת התאריך הנוכחי"
html_title:           "Go: לקבלת התאריך הנוכחי"
simple_title:         "לקבלת התאריך הנוכחי"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## למה: ‏2 משפטים מקיאלים את הסיבה *למה* מישהו יעסוק בקבלת תאריך נוכחי.

עדכון טכנולוגיות ותאריכים באתר או אפליקציה נחוץ למשתמשים. כשמתכנת יעבוד בפרוייקטים שונים, היום הנוכחי הוא דבר שקורה באופן קבוע כדי לוודא שמידע חדש מתווסף ונשמר כראוי.

## איך לעשות זאת: דוגמאות קוד עם פלט בתוך בלוקי קוד "```Go ... ```"

קוד:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	currentDateTime := time.Now()

	fmt.Println("The current date and time is:", currentDateTime.Format("Mon, 02 Jan 2006 15:04:05 MST"))
}
```

פלט:
```
The current date and time is: Mon, 01 Feb 2021 12:30:45 EST
```

קוד זה מדגים כיצד לקבל את התאריך הנוכחי בפורמט מסוים, ניתן להתאים את הפורמט לפי צרכי המשתמש. ישנן גם מגוון אפשרויות לעבוד עם תאריכים ושעות לצורכי חישובים ושינויים.

## Deep Dive: מידע עמוק על קבלת תאריך נוכחי

בשפת Go, קיימת ספריה מובנית בשם "time" המאפשרת עבודה עם תאריכים ושעות באופן נוח ויעיל. הפעלת הפונקציה time.Now() תחזיר את התאריך והשעה הנוכחיים בתור אובייקט מסוג time.Time. ניתן להשתמש במתודות נוספות כמו Format() על מנת להתאים את התאריך לפורמט זמן מסוים או לבצע מחשבות חישוביות. כמו כן, ניתן לשנות את התאריך והשעה הנוכחיים באמצעות מתודות כמו Add() ו-Subtract().

## ראה גם:

- [Time package - The Go Programming Language](https://pkg.go.dev/time)
- [Get Current Date and Time in Go](https://www.golangprograms.com/get-current-date-and-time-in-golang.html)
- [Working with Dates and Times in Go](https://octoperf.com/blog/2018/08/29/go-dates-times/#Time_zone_handling)