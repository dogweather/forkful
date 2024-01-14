---
title:                "Go: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

#מדוע

תוך שימוש בשפת תכנות Go, ניתן לממש את פונקציית המחיקה של תבנית, המאפשרת מחיקת תווים התואמים לתבנית ספציפית. כך ניתן לנקות טקסט תוך שמירה על העיצוב הנכון של הטקסט.

#איך לבצע

המחיקה של תווים התואמים לתבנית בשפת Go נעשית באמצעות הפונקציה DeleteAllString, כאשר כל תו או רצף תווים שתואם לתבנית יימחקו מהטקסט. לדוגמה:

```Go
func main(){
    str := "Hello World!"
    newStr := strings.DeleteAllString(str, "l")
    fmt.Println(newStr)
}
```
פלט:
```
Heo Word!
```
ניתן גם לשלב תבנית מתקדמת עם פונקציה זו, כדי למחוק רצף של תווים או תבנית ספציפית. לדוגמה, נמחק את כל המספרים מטקסט:

```Go
func main(){
    str := "Hello123Go456Programming789!"
    newStr := strings.DeleteAllString(str, "[0-9]+")
    fmt.Println(newStr)
}
```
פלט:
```
HelloGoProgramming!
```

#עומק הדברים

המחיקה של תווים התואמים לתבנית היא חלק מניקוי ועיצוב טקסט באופן אוטומטי, המאפשר לתוכנה לטפל בטקסטים שונים בצורה יעילה. בשימוש בפונקציה זו בשפת Go, ניתן לחסוך זמן ומאמץ של הסרת תווים שנכתבו באותו צורה ולנקות את הטקסט בצורה מהירה ומדויקת.

#ראו גם

- [תיעוד רשמי של פונקציית המחיקה בשפת Go](https://pkg.go.dev/strings#DeleteAllString)
- [פונקציות ניקוי ועיצוב טקסט נוספות בשפת Go](https://golang.org/pkg/strings/)