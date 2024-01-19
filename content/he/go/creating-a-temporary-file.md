---
title:                "יצירת קובץ זמני"
html_title:           "C#: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה זה ולמה? 
יצירת קובץ זמני היא פרוצדורת שמצריך את יצירת קובץ שנמחק לחלופין לאחר שהאפליקציה שיצרה אותו מסיימת את שימושה בו. תכנתים משתמשים בה מכיוון שזה מספק שיטה מהירה ובטוחה לתכנת ללא חשש של התנגשות של קובצים.

## איך לעשות:
בקוד ה-Go הבא, אנחנו מייצרים קובץ זמני.
```Go
package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
)

func main() {
	tempFile, err := ioutil.TempFile("temp", "prefix-")
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println("Temporary file created:", tempFile.Name())

	defer os.Remove(tempFile.Name())
}
```
תוצאת הקוד תהיה:
```bash
Temporary file created: temp/prefix-123456
```

## צלילה עמוקה
### היסטוריה
הצורך ליצור קבצים זמניים יצא לאור עם הופעתה של מערכות ההפעלה הראשונות, שצריכות לנהל משאבים באופן זמני באופן מקביל למשתמשים רבים.
### אלטרנטיבות
אלטרנטיבה לשימוש בקובץ זמני היא יצירת קובץ רגיל, אך זה מוביל לפוטנציאל סיכון של בעיות רבות, כמו התנגשות של קובצים ואף אבטחה רעה.
### נתונים מעומקים
ב-Go, פונקציה TempFile מייצרת קובץ זמני חדש בתיקייה הנתונה, פותחת את הקובץ לכתיבה, ומחזירה את הקובץ.
## ראו גם:
- דוקומנטציה אינטרנטית של הGo: [io/ioutil.TempFile](https://pkg.go.dev/io/ioutil#TempFile)
- [יעילות של קבצים זמניים נוספי](https://www.oreilly.com/library/view/linux-system-programming/0596009585/ch04s03.html) בקטע 4.3.2 של הספר "Linux System Programming".
- [מדריך שיפור הביצועים של הקובץ הזמני](https://www.ibm.com/developerworks/aix/library/au-perf_tune/index.html) מ-IBM.