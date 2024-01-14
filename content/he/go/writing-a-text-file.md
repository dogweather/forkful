---
title:    "Go: כתיבת קובץ טקסט"
keywords: ["Go"]
---

{{< edit_this_page >}}

## למה?

תהליך כתיבת קבצי טקסט הינו משמעותי עבור כל מתכנת גו נכון להיום. יצירת קבצי טקסט מאפשרת לנו ליצור ולאחסן מידע בצורה פשוטה ומסודרת, ולעבוד איתו בצורה יעילה יותר מאשר באמצעות תוכנות טקסט רגילות.

## איך לעשות זאת?

סוגיית כתיבת קבצי טקסט הינה רבת מורכבות ומגוונת, אך בגו כתיבת קבצי טקסט פשוטה ומהירה יותר מאשר בשפות תכנות אחרות. הנה כמה דוגמאות לכתיבת קבצי טקסט בגו, עם פלט לדוגמה:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // פתיחת קובץ קיים לכתיבה
    file, err := os.OpenFile("example.txt", os.O_WRONLY|os.O_CREATE, 0644)
    if err != nil {
        // במקרה של שגיאה, תדפיס הודעת שגיאה
        fmt.Println("Error while opening file:", err)
        return
    }
    // כתיבת תוכן לקובץ
    _, err = file.WriteString("זו דוגמה לקובץ טקסט!")
    // אם אין שגיאות, תדפיס הודעה המעידה על הצלחה
    if err == nil {
        fmt.Println("Successfully wrote to file!")
    }
    // ייסגר הקובץ לאחר סיום הכתיבה
    defer file.Close()

    // יצירת קובץ חדש לכתיבה
    newFile, err := os.Create("new.txt")
    if err != nil {
        fmt.Println("Error while creating file:", err)
        return
    }
    // כתיבת תוכן חדש לקובץ
    _, err = newFile.WriteString("זוהי קובץ חדש שנוצר על ידי התכנית!")
    if err != nil {
        fmt.Println("Error while writing to file:", err)
        return
    }
    // ייסגר הקובץ לאחר סיום הכתיבה
    defer newFile.Close()
}
```

פלט:

```
Successfully wrote to file!
```

## צלילה מקיפה

כתיבת קבצי טקסט בגו היא מנגנון מאוד חזק וגמיש. בנוסף לאפשרות ליצור ולעדכן קבצים קיימים,