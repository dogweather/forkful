---
title:                "קריאת פרמטרים משורת הפקודה"
date:                  2024-01-20T17:55:33.818376-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת פרמטרים משורת הפקודה"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת ארגומנטים משורת הפקודה מאפשרת לקבוע אופציות ופרמטרים לתוכנית מבלי לשנות את הקוד עצמו. תכנות כזה מאפשר גמישות רבה ושימוש חוזר בקוד למגוון סיטואציות.

## איך לעשות:
קוד Bash הבסיסי לקריאת ארגומנטים:
```Bash
#!/bin/bash
echo "הארגומנט הראשון שהתקבל הוא: $1"
echo "הארגומנט השני שהתקבל הוא: $2"
echo "כל הארגומנטים שהתקבלו: $@"
echo "מספר הארגומנטים שהתקבלו: $#"
```
פלט לדוגמא כאשר הסקריפט נקרא עם שני ארגומנטים:
```
הארגומנט הראשון שהתקבל הוא: פרמטר1
הארגומנט השני שהתקבל הוא: פרמטר2
כל הארגומנטים שהתקבלו: פרמטר1 פרמטר2
מספר הארגומנטים שהתקבלו: 2
```

## טבילה עמוקה
בימי הדוס הקדומים, פרמטרים נקראו ישירות משורת הפקודה. ב-Linux, שורת הפקודה עדיין משמשת לזה. ארגומנטים מועברים לסקריפטים ותוכניות כמערך של מחרוזות, וניתנים לגישה באמצעות משתנים מיוחדים (`$1`, `$2`, ..., `$@`, `$#`). יש גם אלטרנטיבות כמו `getopt` ו-`getopts` לעיבוד מתקדם יותר של ארגומנטים. פרטי היישום משתנים בהתאם לצרכים: בדיקת תקינות, קריאת אופציות, הגדרת ערכים ברירת מחדל ועוד.

## ראה גם
- טוטוריאלים רשמיים ל-Bash: [GNU Bash Manual](https://www.gnu.org/software/bash/manual/bash.html)
- מדריך מפורט על כתיבת סקריפטים ב-Bash: [Advanced Bash-Scripting Guide](http://www.tldp.org/LDP/abs/html/)