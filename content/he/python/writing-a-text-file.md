---
title:                "כתיבה לקובץ טקסט"
html_title:           "Bash: כתיבה לקובץ טקסט"
simple_title:         "כתיבה לקובץ טקסט"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? / מה ולמה?
כתיבת קובץ טקסט בפייתון מאפשרת שמירת נתונים ליצירת לוגים, קונפיגורציות ועוד. מתכנתים כותבים לקבצים כדי לשמור מידע לשימוש חוזר, שיתוף וניתוח.

## How to: / איך לעשות:
```Python
# פתיחת קובץ לכתיבה
with open('hello.txt', 'w', encoding='utf-8') as file:
    file.write("שלום, עולם!")

# הוספת טקסט לקובץ קיים
with open('hello.txt', 'a', encoding='utf-8') as file:
    file.write("\nהוספת שורה נוספת.")

# קריאת הקובץ
with open('hello.txt', 'r', encoding='utf-8') as file:
    content = file.read()
    print(content)
```
פלט:
```
שלום, עולם!
הוספת שורה נוספת.
```

## Deep Dive / עומק הנושא:
בעבר, קבצי טקסט נכתבו בפונקציות נמוכות יותר כמו `open`, `write`, ו`close`. פייתון הביא את המנגנון `with` לניהול קובצים בצורה נוחה ובטוחה שמבטיחה סגירה אוטומטית של הקובץ. אלטרנטיבות כוללות מודולים כמו `csv` לקבצי CSV ו`json` לעבודה עם JSON. לכתיבת קובץ נדרשת תשומת לב לקידוד התווים כדי לשמור על תאימות וקריאות.

## See Also / ראה גם:
- [הדוקומנטציה הרשמית של פייתון למודול io](https://docs.python.org/3/library/io.html)
- [מדריך לקידוד תווים בפייתון](https://docs.python.org/3/howto/unicode.html)
- [מידע נוסף על מנהל ההקשר with בפייתון](https://docs.python.org/3/reference/datamodel.html#context-managers)