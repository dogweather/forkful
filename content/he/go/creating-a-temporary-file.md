---
title:                "יצירת קובץ זמני"
html_title:           "Go: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## למה
למה ליצור קובץ זמני?
ניתן ליצור קובץ זמני כדי לשמור נתונים או תוכן מסוים עד שהם יימחקו בסוף התהליך.

## כיצד לייצר קובץ זמני ב-Go
עבור יצירת קובץ זמני ב-Go, ניתן להשתמש בפונקציה `ioutils.TempFile`:
```Go
import (
	"io/ioutil"
	"log"
)

func main() {
	// יצירת קובץ זמני בתיקיית העבודה הנוכחית 
	tempFile, err := ioutil.TempFile("", "example")
	if err != nil {
		log.Fatal(err)
	}
	// כתיבת תוכן לקובץ זמני
	_, err := tempFile.Write([]byte("Hello world!"))
	if err != nil {
		log.Fatal(err)
	}
	// סגירת הקובץ ומחיקתו בסוף התהליך
	defer tempFile.Close()
	defer os.Remove(tempFile.Name())
}
```

Output:
```
Temp File created: /Users/username/example12345
```

## חקירה עמוקה
כאשר מזינים ריק בתיבת האחסון הראשונה של `ioutils.TempFile`, הפונקציה תיצור קובץ זמני בתיקיית העבודה הנוכחית בשם המשמש כקובץ זמני. אם מספקים תיבת אחסון נתיב תקין, הפונקציה תיצור את הקובץ זמני במיקום שצוין ותשמור על התנאים שנתנו לפעולה הזו.

## ראה גם
- דוקומנטציה רשמית של אתר המפתחים של Go על `ioutils.TempFile`: https://golang.org/pkg/io/ioutil/#TempFile
- מדריך מבוא על יצירת קבצים זמניים ב-Go: https://tutorialedge.net/golang/temporary-files-go/
- דוגמאות נוספות ליצירת קובץ זמני ב-Go: https://golangcode.com/create-temporary-file/