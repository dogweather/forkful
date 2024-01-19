---
title:                "קריאת קובץ טקסט"
html_title:           "Go: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
קריאה של קובץ טקסט שמשמעה להעביר את כל המידע שמצוי בקובץ ולהציגו בתוך התוכנה שלך. תכנתים הם כדי לאפשר למשתמשי התוכנה שלהם לעשות שימוש במידע מחוץ לתוכנה.

## איך ל:
אז איך אתה קורא לקובץ טקסט בעזרת Go? הצצה מהירה:
בקוד הבא, אנחנו פתחים קובץ בראש הקוד ועוברים על כל שורה עם לולאה `for`.

```Go
package main

import (
	"bufio"
	"os"
	"fmt"
)

func main() {
	file, _ := os.Open("test.txt")

	scanner := bufio.NewScanner(file)

	for scanner.Scan() {
		fmt.Println(scanner.Text())
	}

	file.Close()
}
```
טרמינל יציג:
```Go
Hello from test.txt!
This is the second line
And the final line
```

## הצצה מעמיקה:
הקריאה של קובץ מתחילה בהשהיה של האינפורמציה הנמצאת בקובץ והצגת הפלט. בעבר, היו לנו כמה אפשרויות שונות לקריאה של קובץ הטקסט, כמו `ioutil.ReadFile` או `ioutil.ReadAll`. בגרסה הנוכחית של Go (1.16), אנחנו משתמשים ב-`os.Open` ו`bufio.Scanner`. אתה יכול גם להשתמש ב-`io/ioutil` או ב-`os.ReadFile` אם הדרישות שלך שונות.

## ראה גם:
[מסמך ליתר פרטים לגבי Scanner במאגר המקור של Go](https://golang.org/pkg/bufio/#Scanner)
[דיסקוסיה על השימוש ב-Scanner vs. io.Reader](https://stackoverflow.com/questions/8757389/reading-a-file-line-by-line-in-go)
[שיעור בווידאו לגבי קריאה וכתיבה של קבצים ב-Go](https://www.youtube.com/watch?v=4KkYPbWo2P8)