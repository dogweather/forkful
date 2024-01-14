---
title:                "Go: כתיבת קובץ טקסט"
simple_title:         "כתיבת קובץ טקסט"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## למה

כתיבת קובץ טקסט היא פעולה חשובה ושגרתית בתכנות בשפת Go. היא מאפשרת לנו ליצור ולשמור מידע חשוב על מנת להשתמש בו באופן יעיל ומאורגן.

## איך לעשות זאת

הנה דוגמא פשוטה של כתיבת קובץ טקסט בשפת Go באמצעות הפונקציה "Create()" וכתיבת תוכן לקובץ באמצעות הפונקציה "WriteString()":

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // יצירת קובץ טקסט חדש
    file, err := os.Create("text_file.txt")
    if err != nil {
        fmt.Println(err)
        return
    }
    // כתיבת תוכן לקובץ
    _, err = file.WriteString("זהו קובץ טקסט חדש שנוצר בשפת Go!")
    if err != nil {
        fmt.Println(err)
        file.Close()
        return
    }
    fmt.Println("הקובץ נוצר בהצלחה!")
    // סגירת הקובץ
    err = file.Close()
    if err != nil {
        fmt.Println(err)
        return
    }
}
```

**Output:**

זהו קובץ טקסט חדש שנוצר בשפת Go!

יתכן שתצטרכו להשתמש בפונקציות נוספות כדי לכתוב לקובץ טקסט מורכב יותר. כדי להיות מוכנים לכתוב קבצים טקסט מתוחכמים יותר, עדיף להתנסות עם כמה דוגמאות וללמוד עוד על פונקציות כמו "Write()", "WriteAt()" ו- "WriteByte()".

## דיל דייב

כתיבת קבץ טקסט היא פעוטת בסיס וחשובה בתכנות בשפת Go. אבל עם כמה ידע נוסף על תיקיות וכתיבת דגימות במערכת הקובץ, ניתן לחקור תמיד את קבצי הטקסט שתצרכו בפרויקטים שלכם בצורה יותר יעילה ומותאמת אישית.

## ראו גם

- [פונקציות של חבילת "os" בשפת Go](https://gobyexample.com/writing-files)
- [הורא