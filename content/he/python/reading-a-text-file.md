---
title:                "קריאת קובץ טקסט"
html_title:           "Python: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## למה
מכיוון שקריאת קובץ טקסט היא כלי חשוב בתחום התכנות, זה יעזור לך להבין את תוכן הקובץ וליצור אפשרויות לעיבודו בקוד.

## כיצד לעשות זאת
כדי לקרוא קובץ טקסט ב-Python, יש להשתמש בפונקציה open() ולכתוב את שם הקובץ כארגומנט. בצע את הפעולות הבאות בתוך ה- "```Python...```" לקרוא ולהדפיס את תוכן הקובץ:
```
file = open("example.txt")
content = file.read()
print(content)

file.close()
```
כדי לשלוח מידע לקובץ טקסט, ניתן להשתמש בפונקציות כמו write() או append(). לדוגמה:
```
file = open("example.txt", "w")
file.write("זהו דוגמא לטקסט\nלכתוב לקובץ")
file.close()

file = open("example.txt", "a")
file.write("\nולהוסיף לקצה הטקסט הקיים")
file.close()
```

## חקירה עמוקה
עכשיו שלמדנו איך לכתוב ולקרוא מקובץ טקסט, ניתן לחקור על כמה נושאים נוספים. למשל, איך להתמודד עם קבצי טקסט גדולים, איך לעבוד עם קבצים בעברית או איך להתאים את הקוד לפי סוג הקובץ שקוראים.

## ראה גם
- [מדריך למתחילים לקרוא קבצי טקסט ב-Python](https://realpython.com/read-write-files-python/)
- [תיעוד רשמי של פונקציית open() ב-Python](https://docs.python.org/3/library/functions.html#open)
- [מדריך מיוחד לעיבוד טקסט ב-Python](https://docs.python.org/3/library/text.html)