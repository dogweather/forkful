---
title:                "Go: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## למה

כל מי שמתעסק עם פיתוח תוכנה יכול להיתקל במצב שבו הוא צריך לשלוח בקשת HTTP. להכיל מודול בפרויקט שלך שתומך בשליחת בקשות HTTP ועובד יפה יכול להחסיר זמן וכמובן כסף. בפוסט הזה נלמד איך לשלוח בקשת HTTP באמצעות Go.

## כיצד לעשות זאת

הפונקציה `SetRequest()` מאחזרת את כל הפרטים שלנו לשליחת בקשת POST.
```Go
func SetRequest() *http.Request {
	reqBody := bytes.NewBuffer([]byte("This is the request body"))
	req, _ := http.NewRequest("POST", "https://example.com/api", reqBody)
	req.Header.Set("Content-Type", "application/json")
	return req
}
```

לאחר מכן, ניתן לקחת את הבקשה שהתמצאה ולשלוח אותה באמצעות `http.DefaultClient`:
```Go
func main() {
	req := SetRequest()
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		fmt.Println(err)
	}
	defer resp.Body.Close()

	fmt.Println("Response status:", resp.Status)
	fmt.Println("Response headers:", resp.Header)
	body, _ := ioutil.ReadAll(resp.Body)
	fmt.Println("Response body:", string(body))
}
```

פלט:
```
Response status: 200 OK
Response headers: map[Content-Length:[0] Content-Type:[text/html; charset=utf-8] Date:[Mon, 19 Oct 2020 10:00:00 GMT]]
Response body: Example response
```

## העומק שבזה
משליחת בקשות HTTP בקוד הוא חלק חשוב מהפיתוח בכל לולאת חייו. יכול להיות מאוד חשוב להיות מכירים עם שדות השפה, כיצד הם עובדים וכיצד ניתן להשתמש בהם מתוך הקוד שלנו. ישנם גם פרמטרים נוספים שניתן להוסיף לבקשות שלנו, כגון הוספת כותרות או מידע אודות המשתמש.

## ראה גם

- [מדריך לשליחת בקשות HTTP באמצעות Go](https://golang.org/pkg/net/http/)
- [הספרייה "net/http" של Go](https://golang.org/doc/articles/wiki/)
- [פיתוח תוכנה באמצעות Go](https://golang.org/doc/code.html)