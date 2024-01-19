---
title:                "ניתוח HTML"
html_title:           "Arduino: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/parsing-html.md"
---

{{< edit_this_page >}}

## מה ולמה?

אנליזת HTML היא התהליך שבו קוד מחשב מפרש את מבנה ה- HTML בדף אינטרנט. מתכנתים עושים זאת כדי לאפשר עיבוד של מידע מהדף.

## כיצד:

הנה קטע קוד בסיסי ב Go שמנתח קובץ HTML:

```Go
package main

import (
	"fmt"
	"golang.org/x/net/html"
	"os"
)

func main() {
	file, err := os.Open("example.html")
	if err != nil {
		fmt.Printf("error: %v\n", err)
		os.Exit(1)
	}
	defer file.Close()

	tokenizer := html.NewTokenizer(file)

	for {
		tokenType := tokenizer.Next()

		switch {
		case tokenType == html.ErrorToken:
			// הגענו לסוף הקובץ, יציאה מהלולאה
			return
		case tokenType == html.StartTagToken:
			token := tokenizer.Token()

			// טיפול בכל מנות tag 
			fmt.Println("tag name: ", token.Data)
		}
	}
}
```
זה הפלט:
```Go
tag name:  html
tag name:  head
tag name:  title
tag name:  body
tag name:  h1
tag name:  p
```

## צלילה עמוקה

אנליזת HTML התפתחה לאורך השנים כתשובה לצורך של מתכנתים לדלות מקוונים ולנהל מידע באופן אוטומטי. Go היא שפת תכנות המספקת קטעי קוד מוכנים מראש (packages) שמאפשרים ניתוח HTML. חלופות כוללות שפות עם ספריות XML/HTML תואמות, כמו Python או JavaScript.

כאשר אנו משתמשים בפונקציה NewTokenizer, היא מחזירה פונקציה שנותנת לנו גישה לtoken הבא בקובץ.

## ראה גם

טיפים נוספים ועומק נוסף בנושא ניתוח HTML ב Go מוצגים במקורות הבאים:
- מדריך Go הרשמי: [המדריך הרשמי](https://golang.org/pkg/net/html/)
- מידע על החבילה "x/net/html": [GoDoc](https://godoc.org/golang.org/x/net/html)
- מדריך פרקטי לאנליזת HTML ב- Go: [מדריך](https://www.devdungeon.com/content/web-scraping-go)