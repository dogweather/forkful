---
title:                "כתיבה לקובץ טקסט"
date:                  2024-01-19
html_title:           "Bash: כתיבה לקובץ טקסט"
simple_title:         "כתיבה לקובץ טקסט"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבה לקובץ טקסט היא שמירת נתונים טקסטואליים בקובץ במחשב. תכניתנים עושים זאת כדי לשמור היסטוריה, הגדרות, נתונים לעיבוד מאוחר יותר ועוד.

## איך לעשות:
```Go
package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	file, err := os.Create("example.txt") // יצירת קובץ
	if err != nil {
		fmt.Println("Cannot create file:", err)
		return
	}
	defer file.Close() // סגירת הקובץ בסוף התכנית

	writer := bufio.NewWriter(file)
	_, err = writer.WriteString("שלום עולם!") // כתיבה לקובץ
	if err != nil {
		fmt.Println("Cannot write to file:", err)
		return
	}

	err = writer.Flush() // שמירת השינויים
	if err != nil {
		fmt.Println("Cannot flush to file:", err)
	}
}
```
פלט דוגמא: יצירת קובץ example.txt המכיל את המחרוזת "שלום עולם!".

## עיון מעמיק:
בעבר, קבצי טקסט נכתבו תוך שימוש ב-APIs מורכבים או צורות מורכבות של I/O. כיום, בשפת Go, מודולים כמו `os` ו`bufio` מקלים על תהליך זה. ישנן אלטרנטיבות נוספות כגון חבילת `io/ioutil` (המיושנת בגרסאות החדשות של Go), או כתיבה ישירה לקובץ עם `os.Write`. כל אחת מהשיטות כוללת פרטי יישום שונים שיכולים להשפיע על ביצועים ונוחות.

## ראו גם:
- דוקומנטציה של החבילה `os`: https://pkg.go.dev/os
- דוקומנטציה של החבילה `bufio`: https://pkg.go.dev/bufio
- מדריך טוב לעבודה עם קבצים ב-Golang: https://www.golangprograms.com/golang-read-write-file.html
