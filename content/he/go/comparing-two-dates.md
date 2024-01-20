---
title:                "השוואה בין שני תאריכים"
html_title:           "Arduino: השוואה בין שני תאריכים"
simple_title:         "השוואה בין שני תאריכים"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מה ולמה?
‏מהו השוואת שני תאריכים? השוואת שני תאריכים היא פעולה בה משווים בין שני תאריכים שונים כדי לראות האם הם שווים או כדי לקבוע מי מהם הקדם. התכנתים משתמשים בכך לצרכי לוגיקה משלהם, כמו למשל לבדוק האם הגיע הזמן לעדכון, או לסדר את הנתונים לפי התאריך. 

## איך לעשות זאת:
```Go
package main
import (
	"fmt"
	"time"
)

func main() {
	// יצירת שני תאריכים
	t1 := time.Date(2020, time.November, 10, 0, 0, 0, 0, time.UTC)
	t2 := time.Date(2021, time.November, 10, 0, 0, 0, 0, time.UTC)

	// השוואת התאריכים
	if t1.Before(t2) {
		fmt.Printf("t1 (%v) is before t2 (%v)\n", t1, t2)
	} else if t1.After(t2) {
		fmt.Printf("t1 (%v) is after t2 (%v)\n", t1, t2)
	} else {
		fmt.Printf("t1 (%v) is equal to t2 (%v)\n", t1, t2)
	}
}
```
פלט:
```shell
t1 (2020-11-10 00:00:00 +0000 UTC) is before t2 (2021-11-10 00:00:00 +0000 UTC)
```

## צלילה עמוקה:
בעבר ההשוואה בין שני תאריכים הייתה יותר מורכבת, אך בשפת `Go` היא מאפשרת לנו לבצע את הפעולה הזאת בקלות באמצעות הפונקציות `Before()`, `After()` או `Equal()`. בלי שפת `Go`, תוכנתים היו צריכים לתכנת פונקציה משלהם כדי לבצע את השוואת התאריכים. חשוב לציין, שהשוואה של תאריכים צריכה להתבצע תוך כדי דיקאי זמן נכונים כדי למנוע הבנות שגויות. 

## ראו גם:
1. [Go Documentation: time package](https://golang.org/pkg/time/)
2. [Go by Example: Time](https://gobyexample.com/time)

זכרו, הכלי הטוב ביותר של התכנת הוא שלומה של מוחו והיכולת שלו לחשוב באופן לוגי. תמיד חשוב לסמוך על אינטואיציה שלכם.