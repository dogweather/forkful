---
title:                "שינוי האותיות לרישיות במחרוזת"
html_title:           "Bash: שינוי האותיות לרישיות במחרוזת"
simple_title:         "שינוי האותיות לרישיות במחרוזת"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## למה
בשל סגנון הכתיבה המדוזה והגמיש של לשונת Bash, ניתן ליצור קודים פשוטים ויעילים. בכתב הזה נלמד איך להגדיר כתר למחרוזת בכדי להפוך את התווים הראשונים לאותיות גדולות.

## איך לעשות זאת
הנה דוגמאות לקוד ולפלט בשפת Bash:
```Bash
#!/bin/bash
string="שלום לעולם"
echo "${string^}" # שלום לעולם
echo "${string^^}" # שלום לעולם
```
המילה הראשונה במחרוזת תהיה מחוללת כתר אם נשתמש בפקודת `${string^}`, וכל המחרוזת תכתב באותיות גדולות אם נשתמש בפקודת `${string^^}`.

## מעמד עמוק
כאשר משתמשים בפקודה `${string^}` או `${string^^}`, השינוי לא נשמר במשתנה המקורי. כדי לשנות את המשתנה עצמו, יש להשתמש בפקודת בשימוש: `${string^=`שינוי`}`, או `${string^^=`שינוי`}`. ניתן גם להשתמש באותיות גדולות בכתיב "here-string" במקום הפקודה `${parameter^}` ו`${parameter^^}`. ניתן להשתמש גם בפקודת `tr` על מנת להגדיר כתר למחרוזת, למשל באמצעות המכתב `[:lower:]` ו-`[:upper:]`.

## ראו גם
- [GNU Bash מדריך רשמי](https://www.gnu.org/software/bash/)
- [מדורל\לשונת Bash בגדו פתוחה](https://dev.to/awwsmm/lets-learn-bash-together-part-1-3a70)
- [פקודת tr](https://www.computerhope.com/unix/utr.htm)