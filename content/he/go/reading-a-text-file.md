---
title:                "Go: קריאת קובץ טקסט"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## למה
ריכוז כמה מילים מספר למה זה כדאי לקרוא קובץ טקסט.

## איך לעשות זאת
השימוש בדוגמאות קוד ותוצאות דוגמא בתוך קובץ קוד מחובר עם הסימונים ```Go ... ```.

```Go
// פתיחת קובץ טקסט
file, err := os.Open("filename.txt")
if err != nil {
fmt.Println("ארעה שגיאה בפתיחת הקובץ:", err)
}

// קריאת תוכן הקובץ והדפסתו למסך
scanner := bufio.NewScanner(file)
for scanner.Scan() {
fmt.Println(scanner.Text())
}

// סגירת הקובץ
err = file.Close()
if err != nil {
	fmt.Println("ארעה שגיאה בסגירת הקובץ:", err)
}
```

## כיולעמוק
מידע נוסף על קריאת קובץ טקסט ושימוש בפונקציות נוספות, כמו פתיחת קובץ במצב כתיבה וכיצד לעבוד עם מירכיבי הקובץ.

## ראו גם
- [מדריך לתכנות בשפת Go](https://golang.org/doc/)
- [מאמר על פונקציות דגלים בשפה Go](https://blog.golang.org/slices)
- [פרויקט מקור פתוח שמשתמש בשפת Go](https://github.com/golang/go)