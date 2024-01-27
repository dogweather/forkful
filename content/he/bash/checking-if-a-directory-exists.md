---
title:                "בדיקה האם ספרייה קיימת"
date:                  2024-01-19
html_title:           "Arduino: בדיקה האם ספרייה קיימת"
simple_title:         "בדיקה האם ספרייה קיימת"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?
בדיקת קיום דירקטוריה ב-Bash היא פעולה שמאפשרת לנו לודא אם דירקטוריה מסוימת קיימת במערכת הקבצים. תוכניתנים עושים זאת כדי למנוע שגיאות בזמן ריצת תוכנית ולהבטיח שהנתיבים המתאימים זמינים לשימוש.

## איך לעשות:
```Bash
# בדיקה אם דירקטוריה קיימת
if [ -d "/path/to/directory" ]; then
  echo "הדירקטוריה קיימת."
else
  echo "הדירקטוריה אינה קיימת."
fi
```
פלט דוגמא:
```
הדירקטוריה קיימת.
```
או
```
הדירקטוריה אינה קיימת.
```

## עיון מעמיק
הפקודה `[-d]` ב-Bash מבצעת בדיקת דירקטוריה כחלק מהטסטים המובנים לכלי ה-builtin `test`. גרסאות ישנות יותר של שורת הפקודה לא תמיד תמכו בה. אפשרויות חלופיות כוללות שימוש בפקודות כמו `ls` ו-`find`, אבל שימוש ב`[-d]` הוא המקובל והבטוח יותר. בנוסף, הדרך בה שורת הפקודה והסקריפט מטפלים בסוגריים מרובעים ובמרווחים היא קריטית להבנת איך לבצע בדיקות תנאי בצורה נכונה.

## ראה גם
- [Bash test constructs](https://www.gnu.org/software/bash/manual/bash.html#Bash-Conditional-Expressions) - מידע על תנאים ב-Bash ממדריך המשתמש הרשמי.
- [Advanced Bash-Scripting Guide](https://tldp.org/LDP/abs/html/) - מדריך לכתיבת סקריפטים מתקדמים ב-Bash.
- [Stack Overflow](https://stackoverflow.com/questions/tagged/bash) - שאלות ותשובות פופולריות על Bash בפורום Stack Overflow.
