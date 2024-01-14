---
title:    "Go: לבדיקת האם תיקייה קיימת"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## למה

למה לבדוק אם תיקייה קיימת? כאשר אנו מפתחים תוכניות בשפת גו, ייתכן שהתוכנית שלנו תצטרך לגשת לקבצים או תיקיות שנמצאים במחשב. בדיקת קיום של תיקייה היא חלק חשוב של פיתוח התוכניות הללו, שכן על ידי כך אנו יכולים לוודא שהתיקייה או הקובץ שאנו מנסים לגשת אליהם קיימים ומוכנים לשימוש.

## איך לבדוק אם תיקייה קיימת

בשפת גו ניתן לבדוק אם תיקייה קיימת על ידי שימוש בפונקציה `os.Stat`. הפונקציה מחזירה את המסלול המלא של התיקייה ומחזירה שגיאה אם התיקייה לא קיימת. נהרוץ את הקוד הבא כדי לבדוק אם תיקייה קיימת:

```go
package main

import (
    "fmt"
    "os"
)

func main() {
    path := "path/to/directory"
    if _, err := os.Stat(path); os.IsNotExist(err) {
        fmt.Println("תיקייה לא קיימת")
        return
    }

    fmt.Println("תיקייה קיימת!")
}
```

פלט:
```
תיקייה קיימת!
```

## העמקה נוספת

למידע נוסף על איך לבדוק אם תיקייה קיימת בשפת גו ניתן להציץ במדריך הרשמי של גו: https://golang.org/pkg/os/#Stat 

## ראו גם

* https://www.digitalocean.com/community/tutorials/how-to-use-the-os-package-in-go
* https://golangdocs.com/check-if-a-file-or-directory-exists-in-go
* https://opensource.com/article/19/6/golang-directory