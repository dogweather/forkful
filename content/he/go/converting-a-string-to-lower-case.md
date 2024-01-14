---
title:                "Go: כתיבת מחרוזת לאותיות קטנות"
simple_title:         "כתיבת מחרוזת לאותיות קטנות"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

עבור לקרוא בעברית:

## למה

למה כדאי להמיר מחרוזת לאותיות קטנות?

ממש בקצרה, להמיר מחרוזת לאותיות קטנות יכול לסייע בטיפול במחרוזות שמכילות אותיות בשני גודלים שונים ובכך לייעל תהליך עיבוד המידע.

## איך לעשות זאת

הכי פשוט להמיר מחרוזת לאותיות קטנות הוא באמצעות הפונקציה "ToLower" שנמצאת בחבילת המודול "strings" של שפת התכנות גו. הנה דוגמה פשוטה לקוד שיוריד במיוחד את כל אותיות המחרוזת לאותיות קטנות וידפיס את התוצאה:

```Go
package main 

import ( 
	"fmt"
	"strings"
) 

func main() { 
	str := "היי חברים!" 
	lower := strings.ToLower(str) 
	fmt.Println(lower) 
} 
```
פלט:
היי חברים!

## העומק המקור

אם תרצו ללמוד עוד על התהליך של המרת מחרוזות לאותיות קטנות, תוכלו לעיין במדריכים ובמאמרים של גורילה-גו ושל תיעוד הרשמי של שפת התכנות גו:

- [מתחילים עם גו: התחברות ראשונה](https://go101.org/article/first-steps-to-learn-go.html)
- [הבסיסים של גו: מחלקות ומתודות](https://go101.org/article/basic-types-and-operations.html)
- [הדרך הנכונה לשפר ייצוג בגו](https://blog.golang.org/strings)
- [מדריך לפונקציות ומתודות בגו](https://golangbot.com/learn-golang-series/)

## ראו גם

[חבילת המודול "strings" בתיעוד הרשמי של גו](https://golang.org/pkg/strings/)

[מאמר על ההתאמה של מחרוזת לאותיות קטנות על ידי Shea Lutton באתר "The Go Blog"](https://blog.golang.org/strings)

[עוד על מטמון וביצועים בגו בתיעוד הרשמי של גו](https://golang.org/doc/go1.10#cgo_explicit_dependency_on_system_c_libraries)