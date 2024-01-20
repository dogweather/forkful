---
title:                "בדיקה אם ספרייה קיימת"
html_title:           "Go: בדיקה אם ספרייה קיימת"
simple_title:         "בדיקה אם ספרייה קיימת"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?
בתכנות, בדיקה אם ספרייה קיימת מתייחסת לאיתור של מקום ספציפי במערכת הקבצים. תכניתאים בודקים את זה על מנת למנוע שגיאות ריצת זמן או לבדוק אם תיקייה מסויימת קיימת לפני שהם יוצרים את התיקייה.

## איך:
חלק זה מכיל דוגמאות לקוד ופלט כדי להראות איך לבדוק אם ספרייה קיימת ב Go:
```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	dirExists, err := doesDirExist("/path/to/directory")
	if err != nil {
		fmt.Println(err)
	} else {
		fmt.Printf("operation successful - directory exists: %v\n", dirExists)
	}
}

func doesDirExist(dirPath string) (bool, error) {
	info, err := os.Stat(dirPath)
	if os.IsNotExist(err) {
		return false, nil
	}

	return info.IsDir(), err
}
```
פלט:
```Go
operation successful - directory exists: false
```
## צלילה עמוקה:
הגדרת הפונקציה 'os.Stat' שנוצרה בהיסטוריה הארוכה של Go, מחזירה מידע על קובץ או ספרייה (אותו FileIno). במקרה של שגיאה, אם הקובץ או הספרייה לא קיימים, המשתנה 'err' שהיא מחזירה, יהיה מסוג PathError או LinkError.

אלטרנטיבות לפונקציה הזו כוללות את 'os.Dirfs' (שצריכה לטפל בכוננים בלבד) ו 'io.ReadDir' (בגרסאות Go 1.16 ומעלה).

באשר לפרטי היישום, שגיאה מסוג Linkerror או PathError מתרחשת רק כאשר היישום מנסה לגשת לנתיב שלא קיים. במקרה הזה, הפונקציה 'os.IsNotExist(err)' תחזיר true.

## ראו גם:
[איך לבדוק אם ספרייה קיימת ב Go 1.16]()
[הסברים נוספים על os.Stat و os.ErrNotExist]()
[מדריך Go מרוכז בנושא טיפול בשגיאות בשפת Go]()