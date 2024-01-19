---
title:                "הפיכת מחרוזת לאותיות ראשונות גדולות"
html_title:           "Go: הפיכת מחרוזת לאותיות ראשונות גדולות"
simple_title:         "הפיכת מחרוזת לאותיות ראשונות גדולות"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
ה"הפיכת מחרוזת לאותיות גדולות" היא פעולה שבה משנים את כל האותיות במחרוזת לאותיות גדולות. מתכנתים מבצעים את הפעולה הזו כדי להבטיח שקוד מסוים לא יתנהג באופן שונה בהתאם לתווים הגדולים או הקטנים.

## איך לעשות את זה:
הפיכת מחרוזת לאותיות גדולות בשפת Go היא פשוטה מאוד עם שימוש בפונקציה `strings.ToUpper`. הנה הדוגמא:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	text := "Hello, World!"
	fmt.Println(strings.ToUpper(text))
}
```
הפלט של הקוד הזה הוא:
```
HELLO, WORLD!
```

## צלילה עמוקה
ארכיאולוגיה של המחשב מצביעה ששמירה על ההבחנה בין אותיות גדולות לקטנות היתה קלה יותר למחשבים הראשונים. נכון לשפת Go, יש חלופה ל `strings.ToUpper` באמצעות שימוש בלולאה ובפונקציה `unicode.ToUpper`. הבחירה בין השניים מתבצעת אי-פעם בהתאם לצרכים המסוימים.

## ראה גם
- [מסמך הפונקציה strings.ToUpper](https://pkg.go.dev/strings#ToUpper)
- [מסמך הפונקציה unicode.ToUpper](https://pkg.go.dev/unicode#ToUpper)
- [מדריך לשפת Go למתחילים](https://tour.golang.org/welcome/1)