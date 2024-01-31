---
title:                "שרשור מחרוזות"
date:                  2024-01-20T17:34:58.374382-07:00
model:                 gpt-4-1106-preview
simple_title:         "שרשור מחרוזות"

category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## מה ולמה?

צירוף מחרוזות ב-Go זה פשוט לחבר מספר טקסטים לאחד. מתכנתים צריכים לעשות את זה כדי ליצור משפטים מתוחכמים, לבנות קלט למשתמשים או לעבד נתונים באופן דינמי.

## איך לעשות:

```go
package main

import (
	"fmt"
	"strings"
)

func main() {
	// דרך פשוטה לצרף מחרוזות
	hello := "שלום"
	world := "עולם"
	helloWorld := hello + " " + world
	fmt.Println(helloWorld) // תוצאה: שלום עולם

	// עם פונקציה Sprintf של fmt
	sentence := fmt.Sprintf("%s, כיצד הכל? %d + %d זה %d.", helloWorld, 2, 3, 2+3)
	fmt.Println(sentence) // תוצאה: שלום עולם, כיצד הכל? 2 + 3 זה 5.

	// בעזרת בנאי המחרוזות strings.Builder
	var builder strings.Builder
	builder.WriteString(hello)
	builder.WriteString(" ")
	builder.WriteString(world)
	fmt.Println(builder.String()) // תוצאה: שלום עולם
}
```

## טבילה עמוקה:

צירוף מחרוזות הוא אקט בסיסי בכל שפת תכנות, וב-Go יש לו קריירה מרשימה. מגירסת Go הראשונה ניתן להשתמש באופרטור `+` לצירוף פשוט, אבל אם אנחנו רוצים לבצע זאת בצורה יעילה יותר, במיוחד עבור מחרוזות רבות, נשתמש ב-`strings.Builder` שנוסף בגרסה 1.10 של Go. זה נותן לנו גמישות וביצועים טובים יותר מהשימוש באופרטור הפשוט. בנוסף, יש לנו את `fmt.Sprintf` שמאפשר להטמיע משתנים ולפורמט אותם ישירות בתוך המחרוזת.

## ראו גם:

- מסמך המחרוזות בתיעוד הרשמי של Go: [Go Docs - Strings](https://golang.org/pkg/strings/)
- פורום שאלות ותשובות של Stack Overflow בנושא צירוף מחרוזות ב-Go: [Stack Overflow - Concatenating strings in Go](https://stackoverflow.com/questions/tagged/go+string-concatenation)
- כתיבה יעילה של קוד עם strings.Builder בבלוג הרשמי של Go: [Go Blog - strings.Builder](https://blog.golang.org/strings)
