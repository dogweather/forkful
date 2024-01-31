---
title:                "כתיבה לקובץ טקסט"
date:                  2024-01-19
html_title:           "Bash: כתיבה לקובץ טקסט"
simple_title:         "כתיבה לקובץ טקסט"

category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/writing-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבה לקובץ טקסט זה לקחת נתונים ולשמור אותם בקובץ שניתן לקרוא. תוכניות עושות את זה כדי לשמור נתונים לשימוש עתידי, לתעד תהליכים, או לעבוד עם יישומים אחרים.

## איך לעשות:

```Bash
# כתיבה לקובץ חדש עם echo
echo "שלום עולם!" > hello_world.txt

# הוספת טקסט לקובץ קיים
echo "תוספת טקסט" >> hello_world.txt

# שימוש בתוכנית לדוגמה לכתיבת קובץ
cat > example.txt
שורה אחת
שורה שניה
^D # לחיצה על Ctrl+D לסיום הקלט
```
הפלט יהיה יצירת קבצי טקסט `hello_world.txt` ו-`example.txt` עם התוכן המתאים.

## הבנה עמוקה יותר

מאז ימי Unix הראשונים, כתיבה לקובץ טקסט משמשת לדיווח, לוגים וחלוקת נתונים. קיימים עוזרים כמו `tee` לכתיבה והדפסה בו-זמנית, ותוכנות עריכה כמו `vi` או `nano`. ב-Bash, כתיבה לקובץ מתבצעת דרך הפניה (>), הוספה (>>), או באמצעות תוכניות כמו `cat`.

## ראה גם

- [Writing and appending to files in Bash](https://www.gnu.org/software/bash/manual/bash.html#Redirections)
- [Advanced Bash-Scripting Guide](http://www.tldp.org/LDP/abs/html/io-redirection.html)
