---
title:                "טיפול בשגיאות"
date:                  2024-01-26T00:53:50.608620-07:00
model:                 gpt-4-1106-preview
simple_title:         "טיפול בשגיאות"

category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/handling-errors.md"
---

{{< edit_this_page >}}

## מה ולמה?

טיפול בשגיאות ב-Go זה עניין של ללכוד בחן ולהגיב בגבורה לקריסות בזמן ריצה. אנחנו עושים את זה כדי למנוע קריסות ולהבטיח שהתוכניות שלנו עובדות באופן צפוי, גם כאשר הדברים לא הולכים לכיוון שבו רצינו.

## איך לעשות:

Go משתמשת בטיפול מפורש בשגיאות. זה אומר שתבדוק אם פונקציה מחזירה שגיאה בכל פעם שאתה קורא לה. ללא יוצאים מן הכלל. הנה איך זה נראה:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	err := doSomething()
	if err != nil {
		fmt.Println("Uh oh:", err)
		os.Exit(1)
	}
}

func doSomething() error {
	// כאילו משהו השתבש
	return fmt.Errorf("משהו השתבש")
}
```

הרץ את זה, ותקבל:

```
Uh oh: משהו השתבש
```

אבל מה אם זה מצליח?

```Go
func doSomething() error {
	// הפעם הכל טוב
	return nil
}
```

אין הפקת תוצאות. מגניב, אין בשורות זה בשורות טובות.

## עיון מעמיק:

ב-Go, טיפול בשגיאות היה נושא לדיונים. מאז ההתחלה, Go החליטה נגד יוצאי דופן להתקרבות יותר מפורשת, שחלק מהמפתחים אוהבים בגלל הפשטות שלה ואחרים מוצאים אותה פרוזאית. סוג הנתונים המובנה `error` הוא ממשק. כל סוג עם שיטה של `Error() string` עומד בדרישות שלו. זה מתחבר עם אתוס הפשטות והמפורשות של Go.

אלטרנטיבות? יש את הזוג `panic` ו-`recover`, אבל הם עבור מקרים יוצאי דופן (המילה משוחקת) כאשר התוכנית לא יכולה להמשיך. תחשוב על `panic` כמו על כפתור המשיקה שאתה לוחץ כאשר אתה יודע שאין דרך חזרה. שימוש צריך להיות בו חסכני.

לעניינינו של טיפול ראשי בשגיאות, Go 1.13 הציגה את עטיפת השגיאות, מה שהקל על הבנת "שרשרת השגיאה" עם פונקציות כמו `errors.Is()` ו-`errors.As()`.

## ראה גם:

לכל הנוגע לטיפול בשגיאות ב-Go:

- הבלוג של Go על טיפול בשגיאות: [https://blog.golang.org/error-handling-and-go](https://blog.golang.org/error-handling-and-go)
- Effective Go – סעיף על טיפול בשגיאות: [https://golang.org/doc/effective_go#errors](https://golang.org/doc/effective_go#errors)
- תיעוד עטיפת השגיאות של Go 1.13: [https://golang.org/doc/go1.13#error_wrapping](https://golang.org/doc/go1.13#error_wrapping)
- פוסט של Dave Cheney על אסטרטגיות טיפול בשגיאות: [https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully](https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully)
