---
title:                "Go: חילוץ תת-מחרוזות"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## מדוע

תיחת המחרוזת היא כלי חשוב בתכנות בשפת גו. היא מאפשרת לנו לקבל חלק מתיחת המחרוזת המקורית, מה שעלול להיות מועיל כאשר אנו צריכים לעבוד עם מחרוזת מסוימת או להפעיל עליה פעולות מסוימות.

## כיצד לבצע

תיחת מחרוזת בשפת גו נעשית באמצעות הפונקציה `strings.Substring()`. הוא מקבל כארגומנטים את המחרוזת המקורית, את תחילת וסוף התיחת ומחזיר את החלק הרלוונטי ממנה. ניתן לראות דוגמאות לשימוש בפונקציה זו בתוך הקוד הבא:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	// תיחת חלק מתיחת מקורית
	str := "זהו מחרוזת מקורית לדוגמה"
	substr := strings.Substring(str, 5, 13)
    fmt.Println(substr)
	// Output: "מחרוזת מקורית"

	// תיחת כמה תווים מתחת לאינדקס מסוים
	str2 := "זהו מחרוזת מקורית לדוגמה"
	substr2 := strings.Substring(str2, 10, len(str2))
    fmt.Println(substr2)
	// Output: "מקורית לדוגמה"
}
```

## העמקה

חשוב לציין שפונקציית `Substring()` מחזירה את התיחת המבוקש ואינה משנה את המחרוזת המקורית במקום. זאת אומרת שאם נעבוד עם התיחת המבוקשת כדי לשנות אותה, עלינו לשמור על המחרוזת המקורית במשתנה נפרד. כמו כן, עלינו לוודא שהאינדקסים שלנו מתאימים, כן לכלול את התו האחרון של התיחת בחישבון.

## ראה גם

* [פונקציית `Substring()` בתיעוד של Go](https://golang.org/pkg/strings/#Substring)
* [מדריך לשפת גו בעברית](https://golang.org/doc/tutorial/)
* [מדריך רשמי לשפת גו](https://tour.golang.org/welcome)