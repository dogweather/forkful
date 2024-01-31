---
title:                "שליחת בקשת HTTP עם אימות בסיסי"
date:                  2024-01-20T18:01:41.395544-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP עם אימות בסיסי"

category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP עם אימות בסיסי מאפשרת לנו להוסיף שכבת אבטחה פשוטה לבקשות שלנו. תוכניתנים משתמשים בפרוטוקול הזה כדי להגביל גישה למשאבים מסוימים על שרת.

## איך עושים את זה:
```go
package main

import (
	"fmt"
	"net/http"
	"encoding/base64"
)

func main() {
	client := &http.Client{}
	req, err := http.NewRequest("GET", "http://example.com", nil)
	if err != nil {
		fmt.Println(err)
		return
	}

	// הגדרת משתמש וסיסמה
	username := "user"
	password := "pass"

	// קידוד ל-Base64
	auth := base64.StdEncoding.EncodeToString([]byte(username + ":" + password))

	// הוספת כותרת אימות לבקשה
	req.Header.Add("Authorization", "Basic "+auth)

	// ביצוע הבקשה
	resp, err := client.Do(req)
	if err != nil {
		fmt.Println(err)
		return
	}
	defer resp.Body.Close()
	
	// כאן אפשר לטפל בתגובה
}
```
## צלילה לעומק:
פרוטוקול אימות בסיסי בHTTP קיים כבר שנים רבות. זהו אחד מהשיטות הפשוטות ביותר להוסיף אימות לבקשות HTTP. חשוב לדעת שכיום יש שיטות מאובטחות יותר כמו OAuth, אבל במקרים מסוימים אימות בסיסי עדיין ממלא את התפקיד. שימוש בHTTPS מומלץ להעברת שם המשתמש והסיסמה באופן מאובטח.

## ראה גם:
- [מסמכי האימות של HTTP ב-MDN (Mozilla Developer Network)](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [תיעוד לפונקציות רלוונטיות ב-pkg.go.dev](https://pkg.go.dev/net/http#Request.SetBasicAuth)
