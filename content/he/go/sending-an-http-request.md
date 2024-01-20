---
title:                "שליחת בקשת http"
html_title:           "Bash: שליחת בקשת http"
simple_title:         "שליחת בקשת http"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## מה ולמה?
שליחת בקשת HTTP הן דרך שבה מחשב מבקש מידע משרת באינטרנט. מתכנתים בחירו בשימוש בזה לצורך לתקשר עם שירותים במרחק של כמה קליקים.

## איך לעשות:
בואו נדמיין שאנחנו רוצים לשלוח בקשת GET לשרת. ניכתוב את הקוד הבא:

```Go
package main

import (
	"fmt"
	"io/ioutil"
	"net/http"
)

func main() {

	resp, err := http.Get("http://example.com")
	if err != nil {
		panic(err)
	}
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		panic(err)
	}
	fmt.Println(string(body))
}
```
הקוד מבצע בקשת GET ל- "http://example.com" ומדפיס את התגובה.

## צלילה עמוקה:
בהיסטוריה, FTP ו SMTP היו דרכים נפוצות לקבלת מידע משרתים. היום, HTTP מאפשר גישה קלה ונוחה לכך.

קיימות אפשרויות חלופיות לביצוע בקשות HTTP ב-Golang, כמו גורמים ממותאמים אישית או מעטפת "goreq". הם מאפשרים יותר שליטה, אך דורשים יותר קוד.

הפרטים של איך רשת מטפלת בבקשת HTTP משנה מבחינה של קיבולת. זו אחת הסיבות שהכוח המעבדתי של השרת ותגובת הרשת נמדדים בסוכנויות אבטחה מחשבים.

## ראו גם:
מסמך ה-Golang הרשמי על שליחת בקשות HTTP: //golang.org/pkg/net/http/#

פורומים בהם מתכנתים מדברים על Golang ובקשות HTTP:
 - Stackoverflow: //stackoverflow.com/questions/tagged/golang+http
 - Reddit: //www.reddit.com/r/golang/