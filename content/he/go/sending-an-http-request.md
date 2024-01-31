---
title:                "שליחת בקשת HTTP"
date:                  2024-01-20T18:00:06.509220-07:00
model:                 gpt-4-1106-preview
simple_title:         "שליחת בקשת HTTP"

category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP היא בעצם הדרך שבה התוכנה שלך מבקשת מידע או שירות משרת אינטרנט כלשהו. תוכניות עושות זאת כדי לתקשר עם ממשקי תכנות אפליקטיביים (APIs), לטעון דפי אינטרנט או לשלוח נתונים לשרת.

## איך לעשות:
```go
package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
)

func main() {
	resp, err := http.Get("https://jsonplaceholder.typicode.com/posts/1")
	if err != nil {
		log.Fatal(err)
	}
	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		log.Fatal(err)
	}
	
	fmt.Println(string(body))
}
```
פלט דוגמה:
```
{
  "userId": 1,
  "id": 1,
  "title": "sunt aut facere repellat provident occaecati excepturi optio reprehenderit",
  "body": "quia et suscipit\nsuscipit recusandae consequuntur expedita et cum\n... (text truncated) ..."
}
```

## עיון מעמיק:
שליחת בקשות HTTP קיימת מאז תחילת האינטרנט, עם הטמעת הפרוטוקול הראשון, בשנות ה-90. בעולם הגו, קיימים חבילות נוספות כדי לשלוח בקשות ללא `http` סטנדרטית, כמו `gorequest`. בקשת HTTP יכולה להיות גם GET, POST, PUT, DELETE, ועוד, בהתאם לפעולה שרוצים לבצע. כשאנחנו משתמשים ב`http.Get`, אנו מניחים שהגוף של התגובה יהיה תמיד בקריאה נשית, אבל פעמים רבות נדרשות פעולות נוספות כמו קביעת כותרות מידע (headers) או שימוש בפרוטוקולים מאובטחים כמו HTTPS.

## ראה גם:
- חבילת `http` רשמית בתיעוד של גו: [net/http package](https://pkg.go.dev/net/http)
- מדריך לחבילות HTTP של צד שלישי: [Awesome Go HTTP](https://awesome-go.com/#http-clients)
