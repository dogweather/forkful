---
title:                "שרשור מחרוזות"
html_title:           "Go: שרשור מחרוזות"
simple_title:         "שרשור מחרוזות"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## למה

אנחנו בעצם משתמשים בזרם הכרטיסיות כדי להשוות את עצמם לאחד הרבה דברים כדי לנסות להבין את הקוד של שפת תכנות שונות.
נרצה לחבר קטעי טקסט למטרה זה. לחיבור קטעים טקסט עוזר לנו ליצור הודעות רחבות וברורות בקוד שלנו כדי להסביר את הצעדים השונים.

## איך לעשות זאת

בGO, אנחנו יכולים לאחד קטעי טקסט באמצעות הפונקציה "Concatenate". אנחנו משתמשים בפונקציה כדי לחבר קטעי טקסט כדי ליצור הודעות מובנות יותר ולשפר את קוד השפה.

 ```Go
func Concatenate(str1 string, str2 string) string {
     return str1 + str2
 } 
 ```

לדוגמה, אם אנחנו רוצים לחבר את המונח "שלום" עם המונח "עולם" כדי ליצור הודעה, אנחנו יכולים לעשות זאת כך:

 ```Go
 msg := Concatenate("שלום", "עולם")
 fmt.Println(msg)
 // Output: שלום עולם
 ```

## טפסים מעמיקים

תהליך חיבור טקסט כמו כן יכול לשמש כדי ליצור מחרוזות גדולות יותר. אנחנו יכולים להשתמש בפונקציה "Concatenate" במקרים פרטיים כשאנחנו צריכים לחבר יותר ממחרוזת אחת ואז נחזור לזה שוב. יש לנו גם אפשרות להשתמש בפונקציות קודויות אחרות מאחורי הקלעים כדי לחבר קטעי טקסט יותר מצורת הפונקציה הפשוטה שעלה לעין.

## ראה גם

- [How to Concatenate Strings in Go - Tutorial by DigitalOcean](https://www.digitalocean.com/community/tutorials/how-to-concatenate-strings-in-go)
- [Strings Package in Go - Reference by Golang](https://golang.org/pkg/strings/)