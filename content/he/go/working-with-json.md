---
title:                "עובדים עם JSON"
html_title:           "Go: עובדים עם JSON"
simple_title:         "עובדים עם JSON"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/working-with-json.md"
---

{{< edit_this_page >}}

## למה

נעשה פתיחה נכונה ונציין שהעיקר במאמר הוא על JSON וכתיבת קוד ב-Go. אנחנו כאן כדי ללמוד איך לעבוד עם נתונים מבני כתובת וניתוחים בפורמט JSON תמיכה מלאה בשפת Go (הגרסה הנוכחית).

## איך לעשות זאת

פעולת הפורמטים נתונים נכתבים בו-קומפקטיים, נוחים ונקיים כמו השפה עצמה, Go. קוד הדגמה לכתיבת קטע קוד JSON והפלט המצורף היא:

```Go
package main

import (
    "encoding/json"
    "fmt"
)

func main() {
    type Person struct {
        Name string `json:"name"`
        Age int `json:"age"`
    }
    
    p := Person{Name: "John", Age: 35}
    b, _ := json.Marshal(p)
    
    fmt.Println(string(b))
}
```

פלט:

```Go
{"name":"John","age":35}
```

## חפירה עמוקה

פורמט JSON יכול להיות מסובך, ולכן יש לכתוב פרק כזה "עמוק" לכיוון שבו נוכל להתייחס לעצמים קשורים נהולים בפורמט JSON ולטפל בהם כמצופה עם הספרייה encoding/json ב-Go. ניתן לכתוב ולקרוא פונקציות עבור אותו פורמט כדי לייצג נתונים בצורה מובנית ולוודא כי הפרמטים לא ייובאו או ייוצאו עם יעדים ריקים ריקים.

## ראו גם

רשימת קישורים שיכולים להוות כלים נוספים לימוד JSON ועבודה עם פורמט זה בשפת Go:

- מדריך התחלה ל-JSON ב-Go: https://www.codewithgo.com/post/go-json/
- ספריה רשמית בשפת Go לניתוח ויצירת JSON: https://pkg.go.dev/encoding/json
- עורך JSON באינטרנט מומלץ: https://jsoneditoronline.org/