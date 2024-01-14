---
title:    "Fish Shell: המרת תאריך למחרוזת"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## למה
יצירת מילון בנתונים המכיל תאריך יכול לעזור לך לבצע חישובים מורכבים ולתכנן משימות עתידיות על פי התאריך הנתון. בשימוש בתכנות בשפת Fish Shell, אתה יכול להמיר תאריך למחרוזת בקלות ובפשטות.

## איך לעשות זאת 
תוכל להמיר תאריך למחרוזת באמצעות הפקודה `date` והתכונה `%d/%m/%Y` (יומן/חודש/שנה). לדוגמה, באמצעות השורה הבאה בתכנית ה-shell שלך תוכל להמיר את התאריך הנוכחי בפורמט מבוקש: 

```Fish Shell
date +%d/%m/%Y
```

פלט הקוד יהיה בפורמט הבא:
`05/08/2021`

## חקירה מעמיקה
בנוסף לתכונה `%d/%m/%Y`, ישנן תכונות נוספות בפקודת `date` שתוכלו להשתמש בהן כדי להמיר תאריך למחרוזת בפורמט אחר. עבור מידע נוסף ודוגמאות, ניתן לעיין במסמך הרשמי של Fish Shell על הפקודה `date`.

## ראה גם
- [תיעוד הפקודה `date` של Fish Shell](https://fishshell.com/docs/current/cmds/date.html)
- [תיעוד הפקודה `date` של באש](https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#Bash-Builtins)
- [דוגמאות להמרת תאריך למחרוזת בשפת Shell](https://www.2daygeek.com/date-command-examples-linux-unix-bash-configure-manupulate-time-date-multi-format/)