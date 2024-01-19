---
title:                "הדפסת פלט ניפוי שגיאות"
html_title:           "Arduino: הדפסת פלט ניפוי שגיאות"
simple_title:         "הדפסת פלט ניפוי שגיאות"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## מה ולמה?
חשיבה אינטימית של באגים היא חשובה מאוד בפיתוח תוכנות, ולכן אנו מדפיסים מידע נוסף בשורת הפקודה שלנו, זה מה שנקרא "הדפסת פלט לניפוי שגיאות". ההדפסה מספקת לנו מידע מפורט על איפה, מתי ומדוע מתרחשות שגיאות.

## איך לעשות:
עבודתך העיקרית תהיה להדפיס הודעת לוג לקונסולה. להלן מעט דוגמאות:

```Go
package main
import "log"

func main() {
  // Println writes to the standard logger
  log.Println("This is a debug message")
}
```

כאן, הקוד מדפיס `"This is a debug message"` לקונסולה.

```Go
package main
import "os"

func main() {
  // Using Fprintln to write to a file
  file, _ := os.Create("logfile.txt")
  os.Stderr = file
  println("This is another debug message")
}
```
כאן ,`"This is another debug message"` מודפס לקובץ בשם logfile.txt.

## צלילה עמוקה
1) היסטוריה: הפעמים הראשונות שהחלו להשתמש בהדפסת מידע לניפוי שגיאות היו בעת שפת סי (C) עדיין הייתה חדשה.
2) אלטרנטיבות: ישנם דרכים פופולריות אחרות כמו השימוש בספריות לניהול לוגים (Logrus, Zerolog).
3) פרטי הוצאה לפועל: גא Go משתמשת במחלקה Log בחבילת 'log' להדפסת מידע לניפוי שגיאות.

## ראה גם
1) מידע נוסף על כיצד לנהל קבצי לוגים: https://golang.org/pkg/log/
2) תיעוד Go הרשמי: https://golang.org/doc/
3) למידה עומקת על ניפוי שגיאות: https://blog.golang.org/debugging