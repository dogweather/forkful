---
title:                "גירוד מספרים אקראיים"
html_title:           "Haskell: גירוד מספרים אקראיים"
simple_title:         "גירוד מספרים אקראיים"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?

חילוף מספרים אקראיים הוא למעשה תהליך של הפקת מספרים באקראי. מתכנתים משתמשים בה כדי ליצור נתונים לא תנאים ולבדוק מקרים שונים.

## איך?

```Go
package main
  
import (
    "fmt"
    "math/rand"
    "time"
)
  
func main() {
    rand.Seed(time.Now().UnixNano())
    fmt.Println(rand.Intn(100))
}
```
הפלט של הקוד יהיה מספר אקראי בין 0 ל-99.

## נסיעה עמוקה

חילוף מספרים אקראיים התחיל להשתמש בתחילת שנות ה-20, כאשר הוחלט להשתמש בה במחשבים. קיימות גם ביבליות אחרות, כמו "crypto/rand", אך "math/rand" המובנה הוא הכי פשוט לשימוש. פונקציית `Seed` מאפשרת הפקת מספרים אקראיים חדשים בכל פעם שהתוכנית רצה.

## ראו גם

קראו את המסמכים הרשמיים על "math/rand": https://golang.org/pkg/math/rand/
והמדריך המעולה "Go by Example" למעטפת של 'math/rand': https://gobyexample.com/random-numbers