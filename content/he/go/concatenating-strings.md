---
title:                "חיבור מחרוזות"
html_title:           "C++: חיבור מחרוזות"
simple_title:         "חיבור מחרוזות"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## מה זה & למה?

צירוף מחרוזות הוא פעולה שבה משרשרים שני חלקים של טקסט לחלק אחד שהוא השילוב של שניהם. מתכנתים משתמשים בצירוף מחרוזות לבניית מחרוזות טקסט מרכיבים קטנים.

## כיצד לעשות:

הנה מעט דוגמאות קוד ב-Go:

```Go
package main

import "fmt"

func main() {
    var str1 = "שלום, "
    var str2 = "עולם!"
    fmt.Println(str1 + str2)
}
```
במקרה מסויים זה, התוצאה תהיה:
```Go
שלום, עולם!
```

## צלילה עמוקה

1. **הקשר ההיסטורי:** צירוף מחרוזות היא אחת מפעולות החיבור הידידותיות. זה היה כאן מהראשוניות של שפת התכנות.
2. **אלטרנטיבות:** שפת Go מאפשרת חיבור מחרוזות באפשרויות נוספות כמו במקרה של `fmt.Sprintf` או `strings.Join`.
3. **פרטים למימוש:** מימוש צירוף מחרוזות ב-Go מתרחש באופן יעיל, תוך שמירה על לוך הזיכרון.

## ראה גם:

1. [נתיב לדרך עם מבוא היסודי לתכנות ב-Go.](https://tour.golang.org/welcome/1)
2. [מסמך עזר שמתמקד בחיבור מחרוזות ב-Go.](https://golang.org/pkg/strings/#Join)
3. [מרכז שאלות הסבר על אופן השימוש ב- ‘fmt.Sprintf’.](https://stackoverflow.com/questions/37532255/one-or-two-arguments-expected)