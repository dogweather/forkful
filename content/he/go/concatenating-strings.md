---
title:                "Go: זרקוק מחרוזות"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/concatenating-strings.md"
---

{{< edit_this_page >}}

#למה

הצבת תווים בסריקת סטרינגים מאפשרת לנו ליצור טכניקה מאוד יעילה שמאפשרת לנו לתמוך בסריקת ערכים יותר מדויקת בתוך כל סריקה, מכיוון שהערכים מחוברים ליחד.

#איך לעשות

```Go
package main

import "fmt"

func main() {
	name := "דוד"
	age := 30
	occupation := "מפתח תוכנה"
	
	// צירוף מחרוזות עם תווים בבינהם
	greeting := "שלום, קוראים לי " + name + " ואני בן " + age + " ואני " + occupation + "!"
	
	fmt.Println(greeting)
}
```

תוצאה:

```
שלום, קוראים לי דוד ואני בן 30 ואני מפתח תוכנה!
```

#צילום עמוק

צירוף מחרוזות באמצעות סריקת סטרינגים מאפשרת לנו גם להשתמש במשתנים בתוך הסריקה עצמה, מה שמאפשר לנו ליצור סריקה דינמית ומותאמת באופן יעיל למטרות שונות. בנוסף, זה מאפשר לנו לחבר חלקים שונים של המחרוזת יחד, מה שמאפשר לנו ליצור תבניות מאותו סוג בקלות.

#ראה גם

- תיעוד רשמי של סריקת סטרינגים בשפת Go: https://golang.org/pkg/strings/
- קוד מקור של פונקציות סריקת סטרינגים בשפת Go: https://github.com/golang/go/tree/master/src/strings
- פוסט בבלוג של Go על סריקת סטרינגים: https://blog.golang.org/strings