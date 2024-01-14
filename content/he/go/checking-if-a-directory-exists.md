---
title:                "Go: בדיקת קיום תיקייה"
simple_title:         "בדיקת קיום תיקייה"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## למה

מבחינת תכנות, ייתכן שתרצה לבדוק אם תיקייה קיימת לפני שאתה מפעיל פקודות עליה. אלו יכולות לכלול כתיבה וקריאה לקבצים או מחיקת התיקייה עצמה. כדי למנוע שגיאות בריצת הקוד, חשוב לבדוק שהתיקייה קיימת לפני שאתה ממשיך לפעול.

## איך לבדוק אם תיקייה קיימת בקוד Go

קוד זה מדגים כיצד לבדוק אם תיקייה קיימת בגוגל Go:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // משתנה שמכיל את כתובת התיקייה שנרצה לבדוק
    dir := "/home/downloads"

    // פעולת Stat שבודקת אם התיקייה קיימת
    if _, err := os.Stat(dir); err == nil {
        // תיקייה קיימת
        fmt.Println("התיקייה קיימת.")
    } else if os.IsNotExist(err) {
        // תיקייה לא קיימת
        fmt.Println("התיקייה לא קיימת.")
    } else {
        // שגיאה בבדיקת התיקייה
        fmt.Println("שגיאה בבדיקת התיקייה.")
    }
}
```

פלט יופיע בטרמינל כך:

```
התיקייה קיימת.
```

## מעמיקים יותר

כדי לבדוק אם תיקייה קיימת, ניתן להשתמש בפונקציה `os.Stat` כפי שנראה בקוד לעיל. פונקציה זו מחזירה מידע על הקובץ או התיקייה בשם המצוין. אם הקובץ או התיקייה קיימים, הפונקציה תחזיר `nil` כשגיאה.

## ראו גם

- [מדריך לתוכניות תיקייה בג'אבה עם גו](https://www.callicoder.com/java-filesystem-api/# create-a-directory-using-the-files-class-java-6-and-later)
- [תיקיית התיעוד של גובו](https://golang.org/pkg/os/#Stat)