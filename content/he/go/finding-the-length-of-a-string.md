---
title:                "מציאת אורך של מחרוזת"
html_title:           "Go: מציאת אורך של מחרוזת"
simple_title:         "מציאת אורך של מחרוזת"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מדוע

אנשים משתמשים בשפת תכנות Go כדי למצוא את אורך של מחרוזת כי זה יכול לעזור להם לעבוד עם נתונים ותוויות תקינות.

## כיצד

הדרך הכי פשוטה כדי למצוא את אורך של מחרוזת ב-Go היא להשתמש בפונקציה `len()` ולהעביר את המחרוזת כפרמטר. נהפוך לקל יותר על ידי שימוש בקובץ `strings` שמכיל פונקציות מועילות לעבוד עם מחרוזות.

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    // קבלת אורך המחרוזת על-ידי שימוש בפונקציה `len()`
    str := "שלום עולם"
    fmt.Println(len(str)) // 10

    // שימוש בפונקציה `Count()` מתוך החבילה `strings`
    // כדי לקבל את מספר הפעמים שמופיעה מחרוזת ספציפית במחרוזת
    str = "Go הוא שפת תכנות מדהימה"
    fmt.Println(strings.Count(str, "Go")) // 1
}
```

## Deep Dive

פונקציית `len()` מחזירה את מספר התווים במחרוזת, לא כולל מרווחים ותוויות תקינות. ניתן לעבוד גם עם חבילת `unicode/utf8` כדי לקבל את מספר התווים האמיתי במחרוזת.

## ראו גם

- [פונקציית `len()` בתיעוד של Go](https://golang.org/pkg/builtin/#len)
- [פונקציות של מחרוזת בחבילת `strings`](https://golang.org/pkg/strings/)