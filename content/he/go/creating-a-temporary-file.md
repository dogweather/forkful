---
title:                "Go: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# למה:

יצירת קובץ זמני ב-Go יכול להיות רעיון טוב עבור מפתחים שמעוניינים לשמור על נתונים שלא ישמרו לטווח ארוך. זה יכול להיות שימושי לכתיבת קבצי לוגים או לטפל במידע זמני במשך תהליך מסוים.

# כיצד לעשות זאת:

## יצירת קובץ זמני חדש:

```Go
package main

import (
    "fmt"
    "io/ioutil"
    "os"
)

func main() {
    // יצירת קובץ זמני וקביעת התוכן שלו
    tempFile, err := ioutil.TempFile("", "example")
    if err != nil {
        fmt.Println(err)
    }
    defer os.Remove(tempFile.Name())

    // כתיבת נתונים לקובץ זמני
    data := []byte("זוהי נתון זמני המתכנתים יכולים להשתמש בו")
    if _, err = tempFile.Write(data); err != nil {
        fmt.Println(err)
    }

    // קריאת הנתונים מהקובץ זמני
    if _, err = tempFile.Seek(0, 0); err != nil {
        fmt.Println(err)
    }
    output, err := ioutil.ReadAll(tempFile)
    if err != nil {
        fmt.Println(err)
    }
    fmt.Println(string(output))

    // יצירת קובץ זמני עם הסיומת ".txt"
    tempFileTXT, _ := ioutil.TempFile("", "example*.txt")
    defer os.Remove(tempFileTXT.Name())

    // כתיבת נתונים לקובץ זמני עם הסיומת ".txt"
    data = []byte("גם כאן ניתן להשתמש בקבצי זמני עם סיומת ספציפית")
    if _, err = tempFileTXT.Write(data); err != nil {
        fmt.Println(err)
    }
}

// פלט:
// זוהי נתון זמני המתכנתים יכולים להשתמש בו
// גם כאן ניתן להשתמש בקבצי זמני עם סיומת ספציפית
```

## יצירת קובץ זמני בתיקייה ספציפית:

```Go
package main

import (
    "fmt"
    "io/ioutil"
    "os"
)

func main() {
    // יצירת תיקייה זמנית וקביעת מסלולים למיקומים שונים
    tempDir, err := ioutil.TempDir("", "example")
    if err != nil {
        fmt.Println(err)
    }
    defer os.RemoveAll(tempDir)

    // יצירת קובץ זמני בתוך התיקייה הזמנית
    tempFile, err := ioutil.TempFile(tempDir, "example")
    if err != nil {
        fmt.Println(err)
    }
    fmt