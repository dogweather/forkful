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

## מה זה ולמה?

קריאת קובץ טקסט היא פעולה שבה קובץ טקסט מאוחסן על המחשב נקרא ונבדק על ידי תוכנה או תכנית. תוכניותים בעליית עיבוד נתונים טכנולוגיים רבים יכולות לקרוא קבצי טקסט כחלק מתהליך העיבוד שלהן. קריאת קובץ טקסט היא דרך נוחה לקרוא נתונים ולעבד אותם באמצעות כתיבת קוד.

## איך לעשות זאת?

```
Python קובץ = open("example.txt")
print(קובץ.read())
```

פלט:
```
זהו קובץ טקסט דוגמה המכיל מילים ומשפטים.
זהו נתון שאנו רוצים לקרוא ולעבד בתוך תוכנית הפייתון שלנו.

```

## התנסה גבוה

פעם, קריאת קבצי טקסט הייתה יותר מורכבת ודורשת שימוש במספר סמלים ומילים לפעולות כמו פתיחה, קריאה וסגירה של הקובץ. כיום, פייתון משתמשת בפונקציה פשוטה open() כדי לשפר את תהליך הקריאה של קבצי טקסט. בנוסף, ישנם פיתוחים וספריות נוספים שאפשרויות נוספות ופונקציות לקריאת קבצים טקסט.

## ראה גם

פונקצית open() במסמך הרשמי של פייתון:
https://docs.python.org/3/library/functions.html#open

קריאת טקסט עם Python:
https://www.geeksforgeeks.org/reading-writing-text-files-python/

ספריה פופולרית לקריאת קבצי טקסט:
https://pypi.org/project/txt/

ספריה חכמה נוספת לקריאת קבצי טקסט:
https://pypi.org/project/bufftxt/