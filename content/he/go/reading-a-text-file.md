---
title:                "קריאת קובץ טקסט"
date:                  2024-01-20T17:54:46.908175-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת קובץ טקסט"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת קובץ טקסט ב-Go היא ליקט טקסט מקובץ על הדיסק. מתכנתים עושים זאת כדי לטעון נתונים, לעבד קונפיגורציות או לקרוא קלט משתמש.

## איך לעשות:
כדי לקרוא מקובץ, עושים שימוש בספריית הסטנדרטית `io/ioutil`. הנה דוגמה פשוטה:

```Go
package main

import (
	"fmt"
	"io/ioutil"
	"log"
)

func main() {
	// קרא קובץ טקסט למשתנה 'data'
	data, err := ioutil.ReadFile("example.txt")
	if err != nil {
		log.Fatal(err)
	}

	// הדפס את התוכן כמחרוזת
	fmt.Print(string(data))
}
```

תוצאת הדוגמה, אמורה להדפיס את תוכן `example.txt`.

## צלילה עמוקה
בעבר, קריאת קבצים דורשת עבודה יותר מורכבת עם פונקציות נמוכות למיניהן. היום, Go מספקת ספריות עשירות שמקלות על פעולה זו.

חלופות ל-ioutil כוללות השימוש בספריות כמו `os` ו`bufio` לקריאה בצורה יעילה יותר ובשליטה גדולה יותר על הבפר של הקריאה.

ככל שהקבצים גדולים יותר, כך גישות שונות לקריאתם יכולות להשפיע על הביצועים. למשל, קריאה מרובזת (`buffered reading`) יכולה להקטין את העומס על הזיכרון ולשפר את התפוקה.

## ראה גם
- [ioutil.ReadFile documentation](https://pkg.go.dev/io/ioutil#ReadFile)
- [bufio package documentation](https://pkg.go.dev/bufio)
- [os package documentation](https://pkg.go.dev/os)
