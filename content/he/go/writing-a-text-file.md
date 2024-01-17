---
title:                "כתיבת קובץ טקסט."
html_title:           "Go: כתיבת קובץ טקסט."
simple_title:         "כתיבת קובץ טקסט."
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת קובץ טקסט היא פעולה נפוצה בתחום התכנות, שמאפשרת למשתמש ליצור קובץ טקסט פשוט וקריא בפורמט טקסט פשוט מבלי להשתמש בתוכנות נוספות. תוכניות מתכנתים כותבות קבצים טקסט על מנת לאחסן מידע וליצור קשר עם משתמשים.

## איך לעשות:
הכי פשוט ומהיר ליצור קובץ טקסט בשפת Go היא להשתמש בפונקציה `WriteString` עם אובייקט הקובץ הנמצא בתיקיית `os`. ניתן לראות בקוד המתואר מטה דוגמאות לכתיבת קובץ טקסט ולקריאתו בשפת Go.

```Go
package main

import (
	"fmt"
	"io/ioutil"
	"os"
)

func main() {
	// יצירת קובץ טקסט חדש
	file, err := os.Create("new_file.txt")
	if err != nil {
		fmt.Println(err)
		return
	}
	defer file.Close()
	
	// כתיבת טקסט לקובץ
	text := "זהו טקסט חדש שנכתב בשפת Go"
	_, err = file.WriteString(text)
	if err != nil {
		fmt.Println(err)
		return
	}
	
	// קריאת קובץ טקסט 
	byteArray, err := ioutil.ReadFile("new_file.txt")
	if err != nil {
		fmt.Println(err)
		return
	}
	// המרת המערך של בתי המחרוזת למחרוזת
	fmt.Println(string(byteArray))
}
```

פונקציות נוספות כמו `Write`, `WriteString`, ו-`WriteByte` זמינות על מנת להתאים את כתיבת הטקסט לצרכים המיוחדים שלך.

## חפירה עמוקה:
שיטת יצירת קובץ טקסט בשפת Go שימושית ויעילה כיום, אך לפני כן אופן הייצור השגוי היה להשתמש בספרייה `io` לצרכי יצירת וכתיבת קבצי טקסט. רק בגרסת Go המעבר לגרסת 1.12 ספריית ה-`io/ioutil` כלולה בשפת Go. ספרייה זו מאפשרת יצירת קבצים טקסט וכתיבתם עם מתודות קצרות ופשוטות יותר.

אם אתה מעוניין לכתוב קבצי טקסט בשפת Go, ישנן אפשרויות אחרות לשימוש כמו `os.OpenFile` ו-`File.WriteString` או `ioutil.WriteFile`.

## ראה גם:
כדי ללמוד עוד פרטים על כתיבת קובץ טקסט בשפת Go ניתן לבדוק את המפרטים המלאים במדריכים כמו https://gobyexample.com/writing-files ו- https://www.dotnetperls.com/create-file-go.