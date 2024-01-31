---
title:                "כתיבה לפלט השגיאה הסטנדרטי"
date:                  2024-01-19
simple_title:         "כתיבה לפלט השגיאה הסטנדרטי"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבה ל-Standard Error (stderr) מאפשרת לנו לשדר הודעות שגיאה ולוגים בנפרד מהפלט הרגיל. מתכנתים עושים זאת כדי להפריד בין מידע רגיל למידע על תקלות, ולאפשר ניתוח תקלות יעיל יותר.

## איך לעשות:
```Bash
#!/bin/bash
# הדפסה ל-stdout
echo "This is a normal message."

# הדפסה ל-stderr
echo "This is an error message." >&2
```
תוצאת דוגמה:
```
This is a normal message.
This is an error message.
```
פלט השגיאה יופיע בטרמינל אבל ניתן להפנות אותו נפרד על ידי הפניה `2>`.

## עיון מעמיק:
הגדרת ה-standard error ב-UNIX התפתחה כחלק מפילוסופיית הפילוג בין ערוצי זרימת הנתונים שונים. לחלופין, ניתן להשתמש בפקודות כמו `logger` להדפסות ל-log system או בפקודות כמו `>&` להפניה משולבת של stdout וstderr. שימוש נכון ב-stderr מאפשר יצירת סקריפטים שהפלט שלהם יכול להיות מסונן ולשמר את הערות השגיאות לניתוח נפרד.

## ראה גם:
- [Advanced Bash-Scripting Guide](https://www.tldp.org/LDP/abs/html/)
- [GNU Bash documentation](https://www.gnu.org/software/bash/manual/bash.html)
- [The Unix Programming Environment by Kernighan and Pike](https://en.wikipedia.org/wiki/The_Unix_Programming_Environment)
