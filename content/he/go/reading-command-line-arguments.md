---
title:                "Go: קריאת ארגומנטים מהשורת פקודה"
simple_title:         "קריאת ארגומנטים מהשורת פקודה"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## מדוע

קריאת פרמטרים של שורת פקודה ב־Go היא דרך חזקה לקבלת קלט מהמשתמש. זה מאפשר לך להשתמש בתוכנית שלך באופן מיומן יותר ולהתאים את הפעולות לפי הארגומנטים שניתן להעביר.

## איך לעשות זאת

```Go
package main

import (
  "fmt"
  "os"
)

func main() {
  // לקבל ספריה משתנה מהפרמטר הראשון של קלט המשתמש
  library := os.Args[1]
  fmt.Println("נבחרה ספריה:", library)
  
  // לקבל רשימת ספרים מהפרמטר השני של קלט המשתמש
  books := os.Args[2:]
  fmt.Println("רשימת הספרים:", books)
  
  // יציאה:
  // נבחרה ספריה: תורת ההכרח
  // רשימת הספרים: [שלום לבבי ואנדרטה]
}
```

## צוללת עמוק יותר

כאשר קוראים לתוכנית, המפרטים מועברים כסלקציות פרמטרים של שורת הפקודה ב־`os.Args`. המפרטים הראשונים הם שם התוכנית והקובץ המופעל, ומכאן ואילך הם מפרטים נוספים שניתן להעביר על ידי המשתמש. כדי להשתמש בפרמטרים הללו, עליך להפעיל את התוכנית שלך עם הפרמטרים המתאימים.

## ראה גם

- ספריית התיעוד הרשמית של Go: https://pkg.go.dev/
- תכנית לשליחת דוא"ל מהשורה האחרונה ב־Go: https://golangdocs.com/sending-emails-golang
- פרמטרים של שורת הפקודה ב־Go: https://medium.com/rungo/building-a-cli-command-line-interface-in-go-ff8c77110ede