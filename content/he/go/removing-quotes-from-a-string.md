---
title:                "הסרת מרכאות ממחרוזת"
date:                  2024-01-26T03:39:48.420268-07:00
model:                 gpt-4-0125-preview
simple_title:         "הסרת מרכאות ממחרוזת"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?

הסרת ציטוטים ממחרוזת פירושה היפטרות מתווי ציטוט כפולים או בודדים שמעטפים את הטקסט האמיתי שלך. אנו עושים זאת כדי לנקות את הנתונים, למנוע שגיאות ניתוח, או להכין טקסט לעיבוד נוסף ללא התוספת הלא נחוצה של סימני ציטוט.

## איך לעשות זאת:

הנה הדרך הפשוטה לזרוק את הציטוטים לפח בשפת Go:

```go
package main

import (
	"fmt"
	"strings"
)

func removeQuotes(s string) string {
	return strings.Trim(s, "'\"")
}

func main() {
	quotedString := "\"שלום, עולם!\""
	fmt.Println("מקורי:", quotedString)

	unquotedString := removeQuotes(quotedString)
	fmt.Println("לא מצוטט:", unquotedString)
}
```

הפלט יראה כך, הציטוטים נעלמו:

```
מקורי: "שלום, עולם!"
לא מצוטט: שלום, עולם!
```

## צלילה עמוקה

בימים של פעם, כאשר פורמטים של נתונים והחלפתם לא היו מתוקננים, ציטוטים במחרוזות יכלו ליצור בלבול. הם עדיין יכולים, במיוחד ב-JSON או כאשר מכניסים מחרוזות למסדי נתונים. חבילת `strings` ב-Go מגיעה עם פונקציית `Trim`, שמורידה לא רק רווחי לבן אלא כל תו שאינך אוהב.

למה לא Regex? ובכן, `Trim` מהירה יותר למשימות פשוטות, אבל אם המחרוזות שלך משחקות את המחבוא עם ציטוטים במקומות מוזרים, regex יכול להיות הארטילריה הכבדה שלך:

```go
import "regexp"

func removeQuotesWithRegex(s string) string {
	re := regexp.MustCompile(`^["']|["']$`)
	return re.ReplaceAllString(s, "")
}
```

זה כמו לבחור בין מספריים ומסור חשמלי; בחרו את הכלי המתאים למשימה.

## ראו גם

למידע נוסף על חבילת ה-`strings` ועל הכלים העוצמתיים שלה:
- [חבילת strings](https://pkg.go.dev/strings)

להשתמש בעוצמה של ביטויים רגולריים ב-Go:
- [חבילת regexp](https://pkg.go.dev/regexp)

רוצים לצלול לפילוסופיה של הסרת רווחי לבן?
- [שיטת Trim](https://blog.golang.org/strings)
