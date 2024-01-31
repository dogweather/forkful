---
title:                "מציאת אורך מחרוזת"
date:                  2024-01-20T17:47:36.985985-07:00
model:                 gpt-4-1106-preview
simple_title:         "מציאת אורך מחרוזת"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
מציאת אורך של מחרוזת ב-Golang זה פשוט לספור את מספר התווים בה. פרוגרמרים עושים זאת כדי לבדוק את התוכן, לאמת נתונים, או לתכנת לוגיקה מותאמת לאורך המחרוזת.

## איך לעשות:
```Go
package main

import (
	"fmt"
	"unicode/utf8"
)

func main() {
	str := "שלום"
	fmt.Println("Length with len():", len(str))          // Outputs: Length with len(): 8
	fmt.Println("Length with utf8.RuneCountInString():", utf8.RuneCountInString(str)) // Outputs: Length with utf8.RuneCountInString(): 4
}
```
`len(str)` יחזיר את מספר הבתים במחרוזת, ולא בהכרח מספר התווים. `utf8.RuneCountInString(str)` יחזיר את מספר התווים האקטואלי, אשר נכון יותר לשפות עם קידוד UTF-8, כמו עברית.

## צלילה עמוקה:
בהתחלה, `len()` נראתה כדרך הישירה למצוא את אורך המחרוזת, אבל היא נוגעת לבתים, לא לתווים. עם הבנה שבתים לא תמיד מפרשים לתווים יחידים ב-UTF-8, הכרחי היה למצוא שיטה חדשה. בעולם שבו אנגלית אינה השפה היחידה, חשוב לתמוך בקידוד תווים רב לשוני.
הספרייה `unicode/utf8` מספקת פונקציות לעבודה נכונה עם UTF-8. אלטרנטיבה היא להמיר את המחרוזת לרצף של 'runes' (`[]rune(str)`), שמייצג תו בקידוד UTF-8, ואז לספור את האלמנטים.

## ראו גם:
- מדריך Golang למחרוזות ותווים: https://blog.golang.org/strings
- מסמך רשמי על ספריית `unicode/utf8`: https://golang.org/pkg/unicode/utf8/
- תרגום קוד מחרוזת לרשימת runes: https://golang.org/doc/go1#rune
- תיעוד על `len()` ואורך מחרוזת: https://golang.org/pkg/builtin/#len
