---
title:    "Go: הגדלת רצף"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# למה

למה ייתכן שתרצו לכתוב פונקציה שמרימה את כל האותיות במחרוזת למלתא תחתונה.

## איך לעשות

עולם דוגמה לשימוש בפונקציית כיון:

```Go
package main

import (
    "fmt"
    "strings"
)

func capital(str string) string {
    return strings.Title(str)
}

func main() {
    myString := "שלום עולם"
    fmt.Println(capital(myString))
}
```

תוכלו לראות שפונקציית כיון אכן ממלאת את הצורך ומכניסה את האות הראשונה בכל מילה לאות גדולה. פלט התכנית הזו יהיה "שלום עולם".

## חפירה עמוקה

כמו כל פעולה בתחום המחשבים, ישנן אפשרויות רבות למימוש פונקציית כיון. למשל, ניתן להשתמש בביקורת קושי כדי לוודא שהמחרוזת לא מכילה תווים לא רצויים. או שמתבצעת בדיקה שהמחרוזת אכן הוא תקינה ולא ריקה לפני כיון. בנוסף, ניתן לכתוב פונקציה שמכניסה את האות הראשונה בלבד לאות גדולה, או שמאפשרת להכניס יותר מחילוק נכון את המחרוזת.

# ראו גם

- [תיעוד רשמי על פונקציית כיון ב-Go](https://golang.org/pkg/strings/#Title)
- [הדרכות נוספות לכתיבת קוד ב-Go](https://gobyexample.com/) 
- [הדרכה על שימוש בפונקציות בשפת Go](https://golangbot.com/functions/)