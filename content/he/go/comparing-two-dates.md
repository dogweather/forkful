---
title:    "Go: השוואת שתי תאריכים"
keywords: ["Go"]
---

{{< edit_this_page >}}

Go כתיבת פוסט בלוג לפי שגרת קלט קבוע עבור פלט מתמטי

## Why

כמו כל שפת תכנות אחרת, ישנם קשיים כאשר מתיימות הפעולות שלנו בתאריכים בשפת Go. לכן, חשוב לדעת כיצד להשוות בין שני תאריכים כדי לטפל בהם כהלכה.

## How To

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// יצירת שני תאריכים להשוואה
	date1 := time.Date(2021, time.November, 15, 0, 0, 0, 0, time.UTC)
	date2 := time.Date(2021, time.November, 30, 0, 0, 0, 0, time.UTC)

	// השוואת התאריכים לפי השנה, החודש והיום
	if date1.Year() == date2.Year() && date1.Month() == date2.Month() && date1.Day() == date2.Day() {
		fmt.Println("התאריכים זהים")
	} else {
		fmt.Println("התאריכים שונים")
	}
}
```

תוצאה:

```bash
התאריכים שונים 
```

## Deep Dive

בשפת Go ישנם מספר פונקציות ושיטות שניתן להשתמש בהם כדי להשוות בין שני תאריכים. בנוסף, עלינו לקחת בחשבון את ההשפעה של אזורי זמן ושפות מקומיות על התאריכים הניתנים להשוואה. כמו כן, חשוב להתעלם מהשעה ומנקודות הזמן כאשר משווים בין תאריכים כדי לקבל תוצאות מדויקות.

## See Also

- [Documentation for time package in Go](https://golang.org/pkg/time/)
- [Stack Overflow: How to compare two dates in Go](https://stackoverflow.com/questions/34635135/how-to-compare-two-dates-in-go)