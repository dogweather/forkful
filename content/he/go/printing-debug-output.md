---
title:                "Go: הדפסת פלט תיקון שגיאות בתכנות מחשבים"
programming_language: "Go"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## למה

כדי לפתור בעיות ושגיאות בקוד, הדפסת פלט דיבאג (debug output) יכולה להיות כלי חזק ביישומי גו (Go).

## איך לעשות זאת

ראשית, ניתן להשתמש בפונקציות מובנות כגון "println" או "fmt.Printf" כדי להדפיס פלט דיבאג. לדוגמה:

```Go
package main

import "fmt"

func main() {
    var num1 = 5
    var num2 = 2
    fmt.Println("The sum of", num1, "and", num2, "is", num1+num2)
}

// פלט:
// The sum of 5 and 2 is 7
```

ניתן גם להשתמש בחבילות חיצוניות כגון "log" או "logrus" כדי להדפיס פלט דיבאג בצורה יותר מתקדמת ונוחה יותר. לדוגמה:

```Go
package main

import (
    "github.com/sirupsen/logrus"
)

func main() {
    log := logrus.New()
    log.WithFields(logrus.Fields{
        "animal": "dog",
        "color": "brown",
    }).Info("I have a pet")
}

// פלט:
// time="2021-07-29T10:30:00+03:00" level=info msg="I have a pet" animal=dog color=brown
```

## Deep Dive

בנוסף לפלט של נתוני דיבאג, ניתן גם להוסיף הודעות מותאמות אישית וצבעים להדפסת פלט דיבאג על מנת להפוך את התהליך ליותר קריא ומבנה. ניתן גם ליצור רמות שונות של דיבאג בעזרת חבילות כמו "zap" או "logrus".

מומלץ גם להשתמש בפלטים של מערכת ההפעלה כגון "os.Stdout" או "os.Stderr" במידה והדפסת הפלט דיבאג מתבצעת בתוך סביבת עבודה למשתמש.

## ראה גם

- [כתיבה נכונה של קוד בגו](https://blog.golang.org/writing-web-apps-in-go)
- [מדריך לכותרת `fmt` בגו](https://golang.org/pkg/fmt/)