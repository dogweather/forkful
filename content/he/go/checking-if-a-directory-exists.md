---
title:                "בדיקת קיום תיקייה"
html_title:           "Go: בדיקת קיום תיקייה"
simple_title:         "בדיקת קיום תיקייה"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## למה
בגלל שלפעמים עלינו לבדוק אם תיקייה קיימת במחשב שלנו, כדי לוודא שהמידע שאנו מחזיקים יהיה נגיש לנו במהלך פיתוח תוכניות ויישומים.

## כיצד לבדוק אם תיקייה קיימת
אנו יכולים להשתמש בפונקציה המובנית os.Stat() כדי לבדוק אם התיקייה קיימת.

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	// שם התיקייה שאנו רוצים לבדוק
	dir := "myfolder"

	// השתמש בפונקציה os.Stat() כדי לבדוק אם התיקייה קיימת
	_, err := os.Stat(dir)

	// בדוק את השגיאות
	if err == nil {
		// אם התיקייה קיימת, הדפס הודעה מתאימה
		fmt.Println("התיקייה קיימת")
	} else {
		// אם התיקייה אינה קיימת, הדפס הודעה מתאימה
		fmt.Println("התיקייה אינה קיימת")
	}
}
```

פלט:

```
התיקייה אינה קיימת
```

## Deep Dive
בעת ביצוע בדיקת קיום לתיקייה, אנו אמורים לשים לב לכמה דברים חשובים. תחילה, ניתן להשתמש בפונקציה os.PathSeparator כדי לקבוע את הנתיב המתאים עבור מערכת הפעלה. בנוסף, אם התיקייה אינה קיימת, אנו יכולים להשתמש בפקודת Mkdir() כדי ליצור אותה בעתיד.

## See Also
- [פונקציות של חבילת os בגולנג](https://golang.org/pkg/os/)
- [פקודת Mkdir בגולנג](https://golang.org/pkg/os/#Mkdir)