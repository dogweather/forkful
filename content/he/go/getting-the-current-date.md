---
title:                "Go: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## למה

אנשים משתמשים בגו כדי ליצור יישומים ותוכניות שונות בשפה פשוטה וקצרה. השימוש בפקודת 'date' יכול להיות חשוב לתכנון יישומים, כמו לדוגמה לתאריך ייצור, תאריך המעסיק את תוכנה או לפעולות של ניתוח לפני דוח התפתחות.

## איך לעשות זאת

```Go
import "fmt"
import "time"

func main() {
    // קבלת תאריך נוכחי
    now := time.Now()
    // המרת התאריך לתאריך מחרוזת בפורמט נתון
    date := now.Format("2006-01-02")
    // הדפסת התאריך המוצג בפורמט מסוים
    fmt.Println("התאריך הנוכחי הוא:", date)
}
```

הפלט: התאריך הנוכחי הוא: 2021-07-23

## חקירה מעמיקה

פקודת 'date' בגו משתמשת בחבילת time של שפת גו כדי להחזיר את התאריך הנוכחי. החבילה מכילה מספר פונקציות עבור עיבוד תאריכים שונים כגון חישוב השעה, תאריך פורטמטים וכן הלאה. תאריך הנוכחי יכול להיות מחרוזת, כמו גם ערך מבנה ספציפי לפי הפורמטינג שנבחר.

## ראה גם

- [חבילת Time בגו](https://golang.org/pkg/time/)
- [הפתרון שלה לחישוב השעה הנוכחית](https://stackoverflow.com/questions/37226757/getting-current-date-and-time-in-go-programming-language)
- [מדריך למידה על גו](https://tour.golang.org/welcome/1)