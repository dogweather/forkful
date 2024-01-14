---
title:                "Go: ניתוח קוד HTML"
simple_title:         "ניתוח קוד HTML"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/parsing-html.md"
---

{{< edit_this_page >}}

## למה
פעולת ניתוח (Parsing) של קוד HTML היא חשובה כשמעוניינים לאתר את מבנה האתר ולהשתמש בנתונים מתוכו.

## איך לבצע ניתוח HTML ב-Go
הנה דוגמא פשוטה של ניתוח קוד HTML בשפת Go:

```
package main

import (
	"fmt"
	"strings"
	"net/http"
	"io/ioutil"
)

func main() {
	
	// שרתים עם קוד HTML פשוט
	urls := []string{"https://example.com", "https://google.com", "https://github.com"}

	for _, url := range urls {
		res, err := http.Get(url)
		if err != nil {
			fmt.Println("Error:", err)
		} else {
			htmlBytes, _ := ioutil.ReadAll(res.Body)
			htmlString := string(htmlBytes)
			// הדפסת כל טקסט בין תגיי <title> </title> באתר
			titleStartIndex := strings.Index(htmlString, "<title>") + len("<title>") 
			titleEndIndex := strings.Index(htmlString, "</title>")
			fmt.Println(url, ":", htmlString[titleStartIndex:titleEndIndex])
		}
	}
}
```

תוצאות הקוד לכתובות האינטרנט שנתתי כדוגמא:

```
https://example.com : Example Domain
https://google.com : Google
https://github.com : The world's leading software development platform · GitHub
```

## העמקת הנתונים של ניתוח HTML
הנתונים שניתקלים בניתוח קוד HTML מכילים רבים מאוד פרטים וכוללים נתונים כמו כותרות, תמונות, כפתורים, טקסטים ועוד. כדי להשתמש בנתונים בצורה יעילה יותר, יש להשיג ידע נוסף על האופן שבו הם מאוחסנים והאיזון בין נתונים באתרים שונים.

## ראו גם
- [ספרית Go עבור ניתוח קוד HTML](https://github.com/google/go-html-transform)
- [מדריך מפורט של ניתוח קוד HTML בשפת Go](https://www.geeksforgeeks.org/how-to-parse-and-provide-data-from-website-into-csv-file-using-golang/)
- [משתמשות בניתוח קוד HTML לצורכי אופטימיזציה וניתוח נתונים](https://towardsdatascience.com/extracting-data-from-web-pages-with-golang-89ccc1b08b9)