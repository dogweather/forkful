---
title:                "בדיקה האם תיקייה קיימת"
date:                  2024-01-20T14:57:05.773225-07:00
html_title:           "Gleam: בדיקה האם תיקייה קיימת"
simple_title:         "בדיקה האם תיקייה קיימת"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?
בדיקת קיום ספריה היא אימות שספריה מסוימת נמצאת במערכת הקבצים. תכניתיים צריכים לדעת אם ספריה קיימת כדי לקרוא, לכתוב או לשנות קבצים בה.

## איך לעשות:
החלק הזה מראה קוד גולנג שבודק אם ספריה קיימת.

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	dir := "/path/to/your/directory"

	if _, err := os.Stat(dir); os.IsNotExist(err) {
		fmt.Printf("Directory %s does not exist.\n", dir)
	} else {
		fmt.Printf("Directory %s exists.\n", dir)
	}
}
```
הפלט יהיה אחד משניים, תלוי אם הספריה קיימת או לא:
```
Directory /path/to/your/directory does not exist.
```
או:
```
Directory /path/to/your/directory exists.
```

## עומקי הים:
לפני `os.Stat`, הדרך הקלאסית לבדוק ספריה הייתה עם הפונקציה `ioutil.ReadDir`, אבל `ioutil` נפרדה מ-Go בגרסה 1.16 לטובת החבילה `os`. יתר על כן, קיימות אלטרנטיבות ל-`os.Stat`, כגון השימוש ב-`os.IsExist` או טיפול בשגיאה במקרה ונסיון לפתיחת הספריה נכשל.

בקרב מפתחים, זהו משהו שנתקלים בו הרבה - רוצים להבטיח שלא יכתבו על פני קבצים קיימים או להיות תלויים בספריה שלא קיימת. מימוש יעיל ישמור על תוכנה יציבה וימנע קריסות בלתי צפויות.

## גם כדאי לראות:
- תיעוד החבילה `os` ב-Golang: https://pkg.go.dev/os
- הגדרות שגיאה בגולנג: https://blog.golang.org/go1.13-errors
- מדריכים לניהול קבצים וספריות בגולנג: https://golangbot.com/read-files/
