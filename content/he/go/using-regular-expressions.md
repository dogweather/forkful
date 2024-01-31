---
title:                "שימוש בביטויים רגולריים"
date:                  2024-01-19
html_title:           "Bash: שימוש בביטויים רגולריים"
simple_title:         "שימוש בביטויים רגולריים"

category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## מה ולמה?
Regular expressions, או בקיצור regex, זה כלי שמאפשר חיפוש והתאמה של טקסט לפי פטרנים (תבניות). תוכניתנים משתמשים בזה כדי לחפש או לעבד טקסטים באופן יעיל על ידי חוקים מורכבים.

## איך לעשות:
בדוגמאות הקוד הבאות, אנו נראה איך להשתמש בregex ב-Go.

```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	// יצירת regex pattern
	regexPattern := `hello(\w+)`

	// קומפילציה של הpattern לרגקס
	r, _ := regexp.Compile(regexPattern)

	// טקסט לבדיקה
	testString := "helloWorld"

	// בדיקה אם הטקסט תואם לregex
	match := r.MatchString(testString)

	fmt.Println("Match found:", match) // צפוי להדפיס true

	// חיפוש והשגת התאמות
	matches := r.FindStringSubmatch(testString)

	fmt.Println("Full match:", matches[0]) // ההתאמה המלאה: helloWorld
	fmt.Println("Capture group:", matches[1]) // קבוצת לכידה: World
}
```

פלט דוגמא:
```
Match found: true
Full match: helloWorld
Capture group: World
```

## עיון נוסף
Regex הוא מהכלים הוותיקים בעולם התכנות, שורשיו חוזרים לשנות ה-50. יש אלטרנטיבות כמו פרסור של טקסטים בלוגיקה ידנית, אבל regex נותן כלים מאוד חזקים. ב-Go, regex ממומש דרך החבילה "regexp" שכוללת אופטימיזציות מתקדמות כדי להפוך את החיפוש ליעיל יותר.

## לקריאה נוספת
- דוקומנטציה של Go על חבילת regexp: https://pkg.go.dev/regexp
- Regex101 לבדיקת regex בזמן אמת: https://regex101.com
- ספר עזר על regular expressions: "Mastering Regular Expressions" by Jeffrey Friedl
