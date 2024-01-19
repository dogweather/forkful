---
title:                "חיפוש והחלפת טקסט"
html_title:           "Elm: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# חיפוש והחלפה ב-Go: גישה בסיסית אבל חזקה
## מה זה ולמה?: 
חיפוש והחלפה של טקסט הוא תהליך של מציאה מחרוזות טקסט אחת והחלפתן באחרת. מתכנתים משתמשים בו לשינוי טקסט, לאיתור שגיאות ולשרת בתוכניות למנועים מורכבים.

## איך לעשות:
``` Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "Hello, World!"
	fmt.Println(strings.Replace(str, "World", "Go", -1)) 
	// Output: Hello, Go!
}
```
בדוגמה זו, פונקצית strings.Replace מחליפה את "World" ב-"Go". המספר -1 מציין שנדרש להחליף את כל הופעות המחרוזת.

## צלילה עמוקה: 
חיפוש והחלפה של טקסט היא טכניקה מרכזית בתכנות, והיא משמשת מאז התחלת תכנות המחשב. אפשרות אחרת היא regexp (ביטויים רגולריים) שמאפשר חיפוש והחלפה מורכבים יותר. פונקצית strings.Replace של גולאנג מכילה אלגוריתם מאוד מהיר לחיפוש והחלפה, אך היא אינה תומכת בביטויים רגולריים.

## ראה גם:
הכנס ל- [regexp package in Go](https://golang.org/pkg/regexp/) לדוגמאות על השימוש בביטויים רגולריים.   ולמידע נוסף על חיפוש והחלפה מתקדם, ראה [Advanced search and replace](https://source.chromium.org/search?q=Replace&ss=%2Fchromium%2Fchromium%2Fsrc).