---
title:                "Go: כתיבה לתקליט התקנה סטנדרטית"
simple_title:         "כתיבה לתקליט התקנה סטנדרטית"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## מדוע

כתיבה לפלסטר השגיאות של המכשיר המחשור בעזרת שפת Go הינו כלי חשוב כאשר מפתחים יישומים ומטרתו לסייע באיתור ותיקון שגיאות בתהליך הפיתוח. מכיוון שכתיבה לפלסטר השגיאות נמצאת בצורה הרבה יותר נגישה ומהירה מכתיבה לקובץ הלוג הרגיל, הכתיבה לפלסטר השגיאות הינה כלי מועט ידוע אך חשוב ביותר בתהליך הפיתוח בשפת Go.

## איך לכתוב לפלסטר השגיאות בשפת Go

### כתיבה לפלסטר השגיאות בצורה בסיסית:

```Go
import "os"
import "log"

func main(){
    file, err := os.Open("example.txt") // פתיחת קובץ לקריאה
    if err != nil {
        log.Fatal(err)  // כתיבה לפלסטר השגיאות
    }
    // קוד נוסף לקריאת הקובץ וטיפול בשגיאות
}
```

כאשר מאתחלים את התוכנית עם הקוד מעלה, במידה והקובץ "example.txt" אינו קיים, תתבצע כתיבה לפלסטר השגיאות והתוכנית תסיים את ריצתה.

### כתיבה לפלסטר השגיאות בצורה מתקדמת כולל גישה לקבצי לוג:

```Go
import "os"
import "log"

var fileName = "log.txt"

func main(){
    file, err := os.Open("example.txt") // פתיחת קובץ לקריאה
    if err != nil {
        f, err := os.OpenFile(fileName, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0644) // יצירת קובץ לוג
        if err != nil {
            log.Fatal(err) // כתיבת שגיאה אם לא ניתן ליצור את הקובץ לוג
        }
        defer f.Close()
        log.SetOutput(f) // הכנסת הקובץ לוג כפלט לכתיבת התקלות
        log.Println(err) // רישום השגיאה לקובץ הלוג
        log.Fatal("נתקלנו בבעיה בפתיחת הקובץ") // כתיבה לפלסטר השגיאות וסיום התוכנית
    }
    // ק