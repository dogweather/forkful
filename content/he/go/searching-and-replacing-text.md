---
title:                "Go: חיפוש והחלפת טקסט"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## למה

חיפוש והחלפת טקסט הינם פעולות חשובות ורגילות בתכנות בשפת Go. הפעולות הללו מאפשרות לנו לשנות ולעדכן קוד בקלות ולהשפיע על פלט התכנית בדרך אינטואיטיבית ונוחה.

## איך לעשות

בשפת Go ישנם מספר דרכים לבצע פעולה של חיפוש והחלפת טקסט. הנה כמה דוגמאות של שימוש בטכניקות שונות כדי למצוא ולהחליף טקסט בתוך מחרוזת עם הבאר האיכותי של Go.

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	text := "שלום Go קהל ישראל!"
	fmt.Println("מחרוזת נתונים:", text)

	// כדי למצוא טקסט בתוך מחרוזת, ניתן להשתמש בחזרה בהפונקציה strings.Contains
	if strings.Contains(text, "Go") {
		fmt.Println("כן, יש את המילה Go במחרוזת")
	}

	// להחליף טקסט בתוך מחרוזת, ניתן להשתמש בפונקציה strings.Replace
	newText := strings.Replace(text, "שלום", "היי", 1)
	fmt.Println("מחרוזת חדשה:", newText)
}
```

תוצאה:

```
מחרוזת נתונים: שלום Go קהל ישראל!
כן, יש את המילה Go במחרוזת
מחרוזת חדשה: היי Go קהל ישראל!
```

## לחקור עמוק יותר

כעת שנתבונן בעוצם התוכנית, ננסה לחפש ולהחליף טקסט בתוך קובץ שלמה באמצעות ספריית ioutil ומחלקת File של Go.

```Go
package main

import (
	"fmt"
	"io/ioutil"
	"strings"
)

func main() {
	// קוראים קובץ שלם ושמירת התוכן במשתנה string
	file, err := ioutil.ReadFile("שם_הקובץ.txt")
	if err != nil {
		panic(err)
	}
	text := string(file)

	// למצוא טקסט בתוך הקובץ ולהחליף אותו בתוכנית את המילה "שלום" במילה "היי"
	newText := strings.Replace(text, "שלום", "היי", -1)

	// ליצור קובץ חדש עם ה