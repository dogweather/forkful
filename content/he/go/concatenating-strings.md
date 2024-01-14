---
title:    "Go: שרשור מחרוזות"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## למה
משום מה גישת חיבור מחרוזות נחשבת כאחת הפעולות הנפוצות בתכנות בשפת Go. זוהי דרך נוחה ויעילה לשלב חלקים שונים של מחרוזות לצורך הצגת מידע משולב.

## איך להשתמש
כדי לחבר מחרוזות בשפת Go, ניתן להשתמש בפונקציית `+` או בפונקציית `fmt.Sprintf()` עם תבנית מיוחדת. הנה מספר דוגמאות לחיבור מחרוזות בשפת Go:

```Go
// חיבור של שני מחרוזות פשוטות
str1 := "שלום"
str2 := "עולם"
res := str1 + " " + str2
fmt.Println(res) // Output: שלום עולם

// חיבור מחרוזת עם מספר שלם
num := 5
str := "המספר שלי הוא"
res := fmt.Sprintf("%s %d", str, num)
fmt.Println(res) // Output: המספר שלי הוא 5
```

## Deep Dive
מעבר לפעולות הפשוטות של חיבור מחרוזות, ישנם עוד אפשרויות מתקדמות ומעניינות לשלב מחרוזות בשפת Go. ניתן להשתמש בפונקציית `strings.Join()` לחיבור מערך של מחרוזות למחרוזת יחידה. ניתן גם להשתמש בתוכניות `bytes.Buffer` כדי ליצור מחרוזת ממקורות שונים בצורה יעילה יותר.

## ראה גם
- [מדריך מלא על חיבור מחרוזות בשפת Go](https://gobyexample.com/string-concatenation)
- [תיעוד רשמי של פונקציית strings.Join()](https://golang.org/pkg/strings/#Join)
- [דוגמה לשימוש בתוכנית bytes.Buffer](https://medium.com/johnhaderlein/go-bytes-buffer-example-fc57a8a27e8a)