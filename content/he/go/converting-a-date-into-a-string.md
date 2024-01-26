---
title:                "המרת תאריך למחרוזת"
date:                  2024-01-20T17:37:01.515222-07:00
model:                 gpt-4-1106-preview
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
מה ולמה?
המרת תאריך למחרוזת היא תהליך שבו פורמט של תאריך מומר לטקסט. תכניתנים עושים זאת כדי להציג תאריכים באופן קריא ונוח למשתמש או לאחסון בבסיסי נתונים.

## How to:
איך לעשות:
```go
package main

import (
	"fmt"
	"time"
)

func main() {
	// יצירת אובייקט תאריך
	currentTime := time.Now()
	
	// פורמט המרה למחרוזת
	dateStr := currentTime.Format("2006-01-02 15:04:05")
	
	fmt.Println("התאריך במחרוזת:", dateStr)
}
```
פלט דוגמה:
```
התאריך במחרוזת: 2023-03-15 17:45:30
```

## Deep Dive
טבילת עומק:
ב-Golang, המרת תאריך למחרוזת מתבצעת בעזרת המתודה `Format` של מסוג `time.Time`. המחרוזת שמחזירה קובעת את הפורמט. שימוש ב-"2006-01-02 15:04:05" כך הוא תבנית שקבעה השפה לקביעת פורמטים. קיימים פורמטים חלופיים כמו `RFC3339` או פונקציות צדדיות כמו `strconv` להמרות מחרוזת-מספר ולהפך. לפעמים, רוצים המרה למחרוזת בשפה מסוימת - ניתן להשתמש בחבילות כמו `golang.org/x/text` לתמיכה בלוקליזציה.

## See Also
ראו גם:
- [Package time](https://pkg.go.dev/time)
- [Package strconv](https://pkg.go.dev/strconv)
- [Go by Example: Time Formatting / Parsing](https://gobyexample.com/time-formatting-parsing)
- [golang.org/x/text](https://pkg.go.dev/golang.org/x/text)
