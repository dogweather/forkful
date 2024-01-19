---
title:                "קריאת קובץ טקסט"
html_title:           "Go: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
קריאה מקובץ טקסט היא התהליך שבו תוכנית מפענחת תוכן מסוים שנשמר בקובץ. מתכנתים מבצעים זאת כדי לייבא מידע שמור מחוץ לתשתית הקוד שלהם.

## איך לעשות:
תראו את הדוגמה הבאה לקריאה של קובץ טקסט ב-Bash:
```Bash
while IFS= read -r line
do
  echo "$line"
done < "file.txt"
```
הפלט היה מחזיר כל שורה מתוך "file.txt".

## צלילה עמוקה
קריאת קבצי טקסט היא אחת העובדות הכי בסיסיות של תכנות. Bash, שהחלה בשנות ה-80, הפשטה את תהליך זה. ישנן שפות אחרות, כמו Python או Node.js, שיכולות להציע אפשרויות אלטרנטיביות אם יש לך צרכים מסוימים. בתוך Bash, קריאת קובץ תלויה בהגדרת מערך IFS (Internal Field Separator), שמגדיר את התווים להפרדה בין שדות בקו של תוכנה.

## ראו גם:
חפשו מידע נוסף באתרים הבאים:
1. [בסיסי Bash Scripting ב-GNU.org](https://www.gnu.org/software/bash/manual/bash.html)
2. [מדריך לקריאת קבצים ב-Python ב-Real Python](https://realpython.com/read-write-files-python/)
3. [המדריך של Node.js לעבודה עם מערכת הקבצים](https://nodejs.dev/learn/the-nodejs-fs-module)