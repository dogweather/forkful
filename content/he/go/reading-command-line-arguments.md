---
title:                "קריאת פרמטרים משורת הפקודה"
date:                  2024-01-20T17:56:33.929538-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת פרמטרים משורת הפקודה"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת ארגומנטים משורת הפקודה מאפשרת למשתמש להעביר פרטים ופרמטרים לתוכנית בעת הרצתה. תכניתיים עושים זאת כדי להגביר את הגמישות ולהתאים את התוכנית לצרכים סציפיים בכל הרצה.

## איך לעשות:
```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	args := os.Args[1:] // תעלם את הארגומנט הראשון שהוא תמיד הנתיב לתכנית
	for i, arg := range args {
		fmt.Printf("ארגומנט #%d: %s\n", i+1, arg)
	}
}
```

פלט לדוגמא:
```
ארגומנט #1: hello
ארגומנט #2: world
```

## צלילה עמוקה
בעבר, קודמי Go השתמשו בספריית flag כדי לנתח ארגומנטים מפורשים יותר. כיום, יש גם חבילות של צד שלישי כמו `cobra` ו-`urfave/cli`, המציעות יותר תכונות ונוחות. שימוש ב-`os.Args` הוא הדרך הישירה והפשוטה ביותר לקבל ארגומנטים, אבל היא לא תמכה בפרמטרים אופציונליים או בפורמט של flags ללא תוספת עיבוד.

## ראה גם
- מדריך לחבילת `flag`: https://pkg.go.dev/flag
- דף בית של `cobra`: https://github.com/spf13/cobra
- דף בית של `urfave/cli`: https://github.com/urfave/cli
- מדריך לחבילת `os`: https://pkg.go.dev/os
