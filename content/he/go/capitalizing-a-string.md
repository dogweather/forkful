---
title:                "הפיכת מחרוזת לאותיות רישיות"
date:                  2024-01-19
html_title:           "Bash: הפיכת מחרוזת לאותיות רישיות"
simple_title:         "הפיכת מחרוזת לאותיות רישיות"

category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
מה זה גדלת אותיות, ולמה זה נחוץ? גדילת אותיות היא המרה של טקסט לאותיות גדולות. תכנתים עושים זאת לשם עקביות, בולטות, או תקנים (כמו תזונתי בתחילת משפט).

## How to:
דוגמאות קוד ופלט דוגמאית תחת בלוקי קוד של Go.
```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	originalText := "שלום עולם!"
	capitalizedText := strings.ToUpper(originalText)

	fmt.Println(capitalizedText)  // שלום עולם!
}
```

פלט דוגמא:
```
שלום עולם!
```

## Deep Dive
ההקשר ההיסטורי: בשפות תכנות רבות, המרת מחרוזות לאותיות גדולות נעשתה לצורך עיבוד טקסט והשוואות, לפני שהיו תקנים להתעלמות מרישיות.
אלטרנטיבות: לGo יש גם את `strings.Title()` ו`strings.ToTitle()` שמגדילות כל מילה או כל תו, בהתאמה.
פרטי יישום: המרת טקסט בGo לאותיות גדולות מתחשבת ביוניקוד ובתווים מיוחדים בשפות שונות.

## See Also
- מסמכי ה-API של הפונקציה `ToUpper`: https://pkg.go.dev/strings#ToUpper
- איך להשוות מחרוזות ברגישות לרישיות: https://golang.org/pkg/strings/#EqualFold
- הבלוג הרשמי של Go על עבודה עם מחרוזות: https://blog.golang.org/strings
