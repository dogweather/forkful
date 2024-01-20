---
title:                "כתיבה לפלט השגיאה הסטנדרטי"
html_title:           "Arduino: כתיבה לפלט השגיאה הסטנדרטי"
simple_title:         "כתיבה לפלט השגיאה הסטנדרטי"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבה לזרם השגיאות הסטנדרטי היא שיטה לשליחת הודעות עבור בעיות ושגיאות בתוכנה. מתכנתים משתמשים בזה כדי להפריד בין פלט רגיל להודעות שגיאה, ובכך ליצור יומני רישום נקיים ויעילים.

## איך לעשות:
```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	fmt.Fprintln(os.Stderr, "הודעת שגיאה: יש תקלה!")
}
```
פלט לדוגמה בקונסול: 
```
הודעת שגיאה: יש תקלה!
```

## צלילה לעומק
בעבר, במערכות UNIX, תכנים נפרדו בין פלט רגיל (stdout) לשגיאות (stderr) לצורך טיפול וניתוח נוח של השגיאות. תחליפים כוללים כתיבה לקובץ לוג מיוחד או שימוש במערכת רישום חיצונית כמו syslog. בעת כתיבה לstderr ב-Go, שימוש בפונקציה `os.Stderr` מתווך לפלט השגיאות המבוקר מנקודת מבט של המערכת.

## ראה גם
- [תיעוד Go לספריית fmt](https://pkg.go.dev/fmt)
- [תיעוד Go לספריית os](https://pkg.go.dev/os)
- [מדריך לניהול שגיאות ב-Go](https://blog.golang.org/go1.13-errors)
- [הבנת זרמי הקלט/פלט סטנדרטיים](https://en.wikipedia.org/wiki/Standard_streams)