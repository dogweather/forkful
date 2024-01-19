---
title:                "המרת תאריך למחרוזת"
html_title:           "Bash: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
הופך את התאריך למחרוזת הוא התרגום של נתוני התאריך, כמו היום, החודש והשנה, לטקסט. מתכנתים משתמשים בכך כדי להציג את המידע למשתמשים ולשמור על אחידות בפורמט תאריך בפרויקטים שלהם.

## איך מבצעים:
על מנת להמיר תאריך למחרוזת ב-Golang, משתמשים בפונקציה Time.Format. נתחיל עם תאריך ונהפוך אותו למחרוזת עם הפורמט "YYYY-MM-DD".

```Go
package main
import (
	"fmt"
	"time"
)
func main() {
	t := time.Now()
	fmt.Println(t.Format("2006-01-02"))
}
```
פלט דוגמה:
```
2022-07-28
```

## צלילה עמוקה:
הפונקציה Time.Format ב-Go התפתחה מהמחלקה SimpleDateFormat ב-Java. לעומת פונקציות אחרות שהשתמשו במבנים מורכבים, פונקציה זו משתמשת בערכים מסוימים שניתן להכניס אותם כ-String. הפורמט מחזורי "2006-01-02 15:04:05" בהתאם ל-API של Go. ישנם דרכים חלופיות להמיר תאריך למחרוזת כמו שימוש בספריות גולנג חיצוניות.

## ראה גם:
* [דוקומנטציה אופיציאלית של Golang ל- Time.Format](https://golang.org/pkg/time/#Time.Format)
* [ספרייה חיצונית להמרת תאריכים ב-Golang](https://github.com/metakeule/fmtdate)
* [המדריך המקיף של Golang לעבודה עם תאריכים וזמנים](https://yourbasic.org/golang/format-parse-string-time-date-example/)