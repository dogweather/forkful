---
title:                "חיפוש והחלפת טקסט"
date:                  2024-01-20T17:58:11.301549-07:00
model:                 gpt-4-1106-preview
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מה ולמה?
תחליף טקסט זה כלי שמשנה טקסט ממשהו למשהו אחר. תוכניתנים עושים את זה לתיקון טעויות, עדכון נתונים או פורמט מחדש של קוד.

## איך לעשות:
להלן קטע קוד בשפת גו שמראה איך לחפש ולהחליף טקסט:

```go
package main

import (
	"fmt"
	"strings"
)

func main() {
	originalText := "שלום עולם! גו זו שפה נהדרת."
	searchFor := "עולם"
	replaceWith := "כולם"

	modifiedText := strings.Replace(originalText, searchFor, replaceWith, -1)
	fmt.Println(modifiedText)
}
```

פלט דוגמא:

```
שלום כולם! גו זו שפה נהדרת.
```

## צלילה עמוקה:
בעבר, לפני ששפות תכנות קיימו פונקציות פנימיות לחיפוש והחלפה, תוכניתים היו צריכים לכתוב את הלוגיקה הזו מאפס. בגו, הסטנדרט הוא החבילה `strings` שכוללת כלים לניתוח ושינוי טקסט. יש גם חבילות אחרות כמו `regexp` להתאמות רגולריות, שהן חזקות יותר אך גם מורכבות יותר לשימוש. חשוב לדעת שהמתודה `Replace` יודעת להחליף כמות ספיציפית של מופעים או את כל המופעים אם תשתמשו ב-`-1`.

## ראה גם:
- תיעוד לחבילה `strings` בגו: [golang.org/pkg/strings](https://golang.org/pkg/strings/)
- שימוש בביטויים רגולריים בגו: [golang.org/pkg/regexp](https://golang.org/pkg/regexp/)
- מדריך למיתוג טקסט וביטויים רגולריים: [regular-expressions.info](https://www.regular-expressions.info/)
