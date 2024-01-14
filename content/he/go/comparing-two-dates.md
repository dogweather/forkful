---
title:                "Go: השוואת שתי תאריכים"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

# למה
בתכנות בשפת Go ישנם רבים מהצורך להשוות בין שני תאריכים. המאמר הזה יציג טכניקות שונות להשוואת תאריכים בשפת Go ויכוון כיצד להשתמש בהן במקרים שונים.

# איך לעשות זאת
כדי להשוות בין שני תאריכים בשפת Go, ישנם כמה מתודות שיכולות לנווט בין אורכים, תאריכי יום בשבוע ועוד. הנה כמה דוגמאות קוד ותוצאות להמחשה:

```Go
package main
import (
    "fmt"
    "time"
)
func main() {
    date1 := time.Date(2020, time.January, 1, 0, 0, 0, 0, time.UTC)
    date2 := time.Date(2021, time.February, 1, 0, 0, 0, 0, time.UTC)
    
    // השווה תאריך עם אחר לפי שנה, חודש ויום החל מהאורך
    if date1.Equal(date2) {
        fmt.Println(date1, "הוא שווה ל", date2)
    } else {
        fmt.Println(date1, "אינו שווה ל", date2)
    }
    
    // הצג את התאריך המוקדם יותר לפי שנה, חודש ויום
    if date1.Before(date2) {
        fmt.Println(date1, "קודם ל", date2)
    } else {
        fmt.Println(date2, "קודם ל", date1)
    }
    
    // הפכת חלקים של תאריך למערך
    dateArray := []int{date1.Year(), int(date1.Month()), date1.Day()}
    fmt.Println("מערך התאריך הוא:", dateArray)
}
```

תוצאות יעילות:

2020-01-01 00:00:00 +0000 UTC אינו שווה ל 2021-02-01 00:00:00 +0000 UTC
2020-01-01 00:00:00 +0000 UTC קודם ל 2021-02-01 00:00:00 +0000 UTC
מערך התאריך הוא: [2020, 1, 1]

# חקירה מעמיקה
בנוסף למתודות הבסיסיות של השוואת תאריכים, תוכלו להשתמש גם בחבילת "time" המכילה פונקציות נוספות כמו "Compare" המאפשרת להשוות בין שני תאריכים על פי תשובת תחילת האותיות או להשתמש בעדיפות למידות כמו "האם תאריך מסוים קודם או לא". כמו כן,