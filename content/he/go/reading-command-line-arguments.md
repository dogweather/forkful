---
title:    "Go: קריאת פרמטרי שורת פקודה"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

וואו: רק 1-2 משפטים המסביר למה מישהו ירצה לקרוא על משתני מזוודות בקו הפקודה.

כיצד לעשות: דוגמאות קוד ופלט מופיעים בתוך קוד המתחיל ב- "```Go ...```" 

```Go
package main

import "fmt"
import "os"

func main() {
	// קריאה של פחות ממשתנה
	arg := os.Args[1] // arg יציג את הארגומנט הראשון שהוא משתנה ב-
	fmt.Println("תיבת הקלט:", arg)

	// קריאה של כל המשתנים
	allArgs := os.Args[1:] // allArgs יציג את כל המשתנים ללא הארגומנט הראשון
	fmt.Println("כל התיבות הנלקחות מטיבת הקלט:", allArgs)
}
```

עומק מעומק: מידע נוסף על קריאת משתנים של טור אחד.

**ראה גם:**

- [פירוט API רשומות נטמעות לערימת טורים מיוחדת](https://golang.org/pkg/flag/)
- [סימני לחץ של תוכניות עצמאיות](https://gobyexample.com/command-line-arguments)
- [למד Golang ב-שישי Tutorials](https://www.youtube.com/playlist?list=PLQVvvaa0QuDfKTOs3Keq_kaG2P55YRn5v)