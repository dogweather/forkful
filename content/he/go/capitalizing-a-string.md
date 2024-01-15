---
title:                "הופעלו פרושים: ניצור מספרים"
html_title:           "Go: הופעלו פרושים: ניצור מספרים"
simple_title:         "הופעלו פרושים: ניצור מספרים"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## למה

בכל תוכנית תאים היא חשוב לתת למשתמש התכנית פלט נכון ומתאים. כאשר משתמשים ב-Golang כדי לכתוב תוכניות, אנו צריכים להקפיד על כך שהפלט יהיה בפורמט נכון ומקורי. ביותר המקרים, נתוני הפלט צריכים להיות רשומים באופן מקורי ומתועד כפי שנקבע. השתמש בפקודת הimport "strings" כדי להשתמש בפונקציות מתוך החבילה הזו שמאפשרת לנו לנתח סדריות תווים. 

## איך לבצע

הנה דוגמאות לקוד המחילות כיצד להשתמש בפונקציה Capitalize שקיימת בחבילת strings על מנת להקפיד על שכפול הפלט לפני ההדפסה.

```Go
import "strings"

func main() {
    // הכנס את התווים לתוך משתנה
    str := "hello world"

    // שכפל את הפלט
    capitalStr := strings.ToUpper(str)

    // הדפס את הפלט
    fmt.Println(capitalStr)

    // הפלט יהיה "HELLO WORLD"
}
```

## צלילה עמוקה

כפי שראינו בדוגמאות הקוד, פונקציית Capitalize מעולם לא משנה את המשתנה המקורי, אלא פשוט מחזירה פלט חדש. לכן, אם ברצוננו לשנות את המשתנה המקורי, יש להשתמש בפונקציית strings.Replace כדי להחליף את התווים בחריץ המקורי בתווים של היו. בנוסף, כדי להבטיח שהפלט יהיה בפורמט מקורי, ניתן להשתמש בפונקציה strings.TrimSpace להסרת הרווחים מהמשתנה.

## ראה גם

- תיעוד החבילה strings של Golang (https://golang.org/pkg/strings/)
- פונקציות אחרות שניתן להשתמש בהן בחבילת strings (https://golang.org/ref/spec#String_functions)