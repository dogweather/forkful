---
title:                "Go: המרת תאריך למחרוזת"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## בכדי למה

למה משתמשים בהמרת תאריך למחרוזת בקוד של Go? בעזרת המחרוזת, ניתן להציג תאריך בפורמט של מחרוזת וכך להתאים את התאריך לצורך מסוים, כגון הצגתו על מסך המשתמש או שמירתו בבסיס נתונים.

## כיצד לעשות זאת

בקוד הבא בשפת Go תוכלו למצוא מספר דוגמאות להמרת תאריכים למחרוזת ואת הפלט שנוצר.

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// תאריך ושעה נוכחיים
	currentTime := time.Now()

	// המרת התאריך למחרוזת בפורמט של התאריך הקודם
	strDate := currentTime.Format("January 2, 2006") // 2 בינואר 2006
	fmt.Println(strDate) // Output: January 8, 2021 (8 בינואר 2021)

	// המרת השעה למחרוזת בפורמט של השעה הקודמת
	strTime := currentTime.Format("15:04:05") // 15:04:05
	fmt.Println(strTime) // Output: 18:30:08
}
```

## Deep Dive

בשפת Go ישנם מספר אפשרויות להמרת תאריך למחרוזת. בכדי להתאים את התאריך לצורך הספציפי שלכם, ניתן להשתמש בפורמטרים שונים תוך השימוש בפקודה `Format()`. בחלק מהמקרים יש להשתמש בפורמטרים שקשורים ליום בחודש, חודש ושנה. בחלקו השני של הקוד ניתן לראות שימוש בפורמטרים הקשורים לשעה, דקות ושניות. לתוספת מידע נוסף על הפורמטרים השונים, ניתן לקרוא את התיעוד המלא של הפקודה `Format()` בכתובת הבאה: https://golang.org/pkg/time/#pkg-constants.

## ראו גם

- תיעוד רשמי של הפקודה `Format()` בשפת Go: https://golang.org/pkg/time/#pkg-constants
- מדריך מפורט על כתיבת תאריכים בשפת Go: https://appliedgo.net/datetime/
- חבילת `time` בשפת Go: https://golang.org/pkg/time/