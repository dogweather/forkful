---
title:                "Go: התחלת פרויקט חדש"
programming_language: "Go"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

# למה

חשיבה על התחלת פרוייקט חדש ב-Go יכולה להיות אתגר מרגש ומאתגר. ישנם הרבה סיבות שיכולות לעורר רצון ליצור משהו חדש, בין אם זה להתחיל הבנת תוכניות פלאטפורמה חדשה או פשוט לעזור לפתח יכולות מתקדמות שלך כמתכנת.

# איך לעשות זאת

איך ניתן להתחיל? הנה כמה דוגמאות של קוד ופלט לחישוב מספרים ראשוניים עם השפה Go:

```
package main

import "fmt"

func main() {
    // קבלת מספר מהמשתמש
    var num int
    fmt.Print("הכנס מספר: ")
    fmt.Scan(&num)

    // ללולאת מספרים עד המספר הנתון
    for i := 2; i <= num; i++ {
        // בדיקה האם המספר הוא ראשוני
        isPrime := true
        for j := 2; j < i; j++ {
            if i%j == 0 {
                isPrime = false
                break
            }
        }
        // הדפסת המספר הראשוני אם מתקיים
        if isPrime {
            fmt.Println(i)
        }
    }
}
```

פלט:

```
הכנס מספר: 10
2
3
5
7
```

# חפירה מעמיקה

תוכלו להשתמש בפרוייקט התחלתי זה כדי להתחיל לחקור יותר בעומק את השפה Go ולמצוא דרכים לשפר את יכולות התכנות שלכם. ניתן להוסיף פונקציות נוספות, לשנות את הקוד כדי לייעל את ביצועיו ולהתחיל לעשות יישומים מתקדמים יותר.

# ראה גם

- [דוגמה לפרוייקט התחלתי ב-Github] (https://github.com/golang/example)
- [התחלה פשוטה עם גורות ה-Golang] (https://golang.org/doc/code.html)
- [מדריך להתחלת פרוייקט חדש עם Go] (https://blog.golang.org/organizing-go-code)