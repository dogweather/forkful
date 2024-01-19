---
title:                "שליחת בקשת http עם אימות בסיסי"
html_title:           "C: שליחת בקשת http עם אימות בסיסי"
simple_title:         "שליחת בקשת http עם אימות בסיסי"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה ולמה?

שליחת בקשת HTTP עם אוטנטיקציה בסיסית היא התהליך שבו יישום מבצע בקשה לשרת עם זיהוי משתמש וסיסמה. מתכנתים עושים זאת כשהם רוצים לבצע בקשת HTTP למשאב שדורש אימות משתמש.

## איך לעשות:

```Go
package main

import (
	"fmt"
	"net/http"
	"encoding/base64"
)

func main() {
	client := &http.Client{}
	req, _ := http.NewRequest("GET", "http://example.com", nil)
	basicAuth := "username:password"
	// הצפנה של שם המשתמש והסיסמה בלוביט SOAP1
	req.Header.Add("Authorization", "Basic "+base64.StdEncoding.EncodeToString([] byte(basicAuth)))
	resp, _ := client.Do(req)
	fmt.Println("Response status:", resp.Status)
}
```
כאשר אתה מבצע את הקוד הזה, אתה צפוי לראות פלט שכזה:

```Go
Response status: 200 OK
```

## צלילה עמוקה 

אוטנטיקציה בסיסית הוא שיטה מעונת 1996 (RFC 1945) המחייבת את השרת לבדוק אם שם המשתמש המסופק וסיסמה תואמים לאלה שקיימים במאגר הנתונים שלו. השיטה הזו עשויה להתרחש באופן בלתי מוחקר, אך היא מהירה ובסיסית. לאחר מכן, השיטות התחליפיות כוללות OAuth, JWT, שימוש ב-API של Google או Facebook ועוד.

פרטי היישום של שליחת בקשת HTTP עם אוטנטיקציה בסיסית מתמקדים גם באיך להוסיף את הכותרת "Authorization" וכיצד למיתג שם משתמש וסיסמה לפורמט Base64.

## ראה גם

1. [מאמר RFC 1945](https://www.ietf.org/rfc/rfc1945)
2. [אוטנטיקציה ב-Go](https://go.dev/play/p/6Wv1KZHxQuV) 
3. [צפינת Base64](https://en.wikipedia.org/wiki/Base64) 
4. [HTTP בקשות עם Go](https://golang.org/pkg/net/http/)
5. [Google API](https://developers.google.com/identity/protocols/oauth2)
6. [Facebook API](https://developers.facebook.com/docs/facebook-login/)