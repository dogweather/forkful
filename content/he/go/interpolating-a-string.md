---
title:                "שרבוב מחרוזת"
date:                  2024-01-20T17:51:25.006516-07:00
model:                 gpt-4-1106-preview
simple_title:         "שרבוב מחרוזת"

category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
אינטרפולציה של מחרוזות זה הדבקה של משתנים או ערכים בתוך מחרוזת. תכנתים עושים את זה כדי לייצר מחרוזות דינמיות בקלות.

## איך לעשות:
בגו, אנחנו משתמשים ב`fmt.Sprintf` כדי להוסיף ערכים למחרוזת. דוגמה:
```go
package main

import "fmt"

func main() {
	name := "גלעד"
	job := "מתכנת"
	sentence := fmt.Sprintf("שמי %s ואני %s.", name, job)
	fmt.Println(sentence)
}
```

פלט:
```
שמי גלעד ואני מתכנת.
```

כמו כן, יש סימון חדשני ב-GO נקרא 'String interpolation' שצץ בגרסה 1.18:
```go
name := "גלעד"
job := "מתכנת"
sentence := fmt.Sprintf("שמי %[1]s ואני %[2]s.", name, job)
fmt.Println(sentence)
```

פלט זהה: 
```
שמי גלעד ואני מתכנת.
```

## עיון מעמיק:
אינטרפולציה של מחרוזות היא כלי ששפות תכנות רבות מציעות כדי לגמישות הטמעת נתונים. בשפות אחרות, כמו PHP או JavaScript, יש סימון פשוט יותר. בגו, לא ממש היה מובנה משהו דומה עד שהגיעה הפונקצייה `fmt.Sprintf`.

יש גם גישות אלטרנטיביות, כמו שימוש בחיבור מחרוזות (+) או בשיטות כמו `strings.Builder` לבנייה אופטימאלית של מחרוזות ארוכות.

אבל עם `fmt.Sprintf`, הקוד הוא עקבי וקריא יותר, וזה הפופולרי ביותר למרות שזה קצת איטי יותר מלחבר מחרוזות ישירות.

## ראה גם:
- תיעוד הפונקציה `fmt.Sprintf` מתוך התיעוד הרשמי של Go: https://pkg.go.dev/fmt#Sprintf
- מבוא ל`strings.Builder` ב-Golang: https://pkg.go.dev/strings#Builder
- דוקומנטציה על String formatting ב-Golang: https://golang.org/pkg/fmt/
