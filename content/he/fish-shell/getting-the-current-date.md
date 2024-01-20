---
title:                "קבלת התאריך הנוכחי"
html_title:           "C#: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?
קבלת התאריך הנוכחי היא פקודה שמחזירה את התאריך והשעה של המערכת בשעת הריצה. פרוגרמיסטים משתמשים בזה לשלל סיבות, כמו לתת תווית זמן למידע הנוכחי או למדד משך זמן של ביצוע משימה.

## איך עושים:
ב-Fish Shell, אפשר לרשום את הפקודה הבאה:
```Fish Shell
echo (date)
```
זה יחזיר את התאריך והשעה בפורמט: 'תאריך שעה'.
לדוגמה:
```Fish Shell
Fri 29 Oct 2021 10:32:58 BST
```

## צלילה עמוקה
קבלת התאריך הנוכחי אינה מסובך, אבל יש לה היסטוריה ואפשרויות מאתגרות. בעבר, זה היה מחויב שימוש ב-APIs מסוימים של מערכת ההפעלה. עם השנים, שפות תכנות פיתחו דרכים פשוטות יותר להשיג את המידע הזה. 
לדוגמה, ב-Python יש אפשרות לקבלת התאריך הנוכחי באמצעות `datetime.datetime.now()`.   
ב-JavaScript יש את `Date()`.

## ראו גם
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Getting the current date in other programming languages](https://www.w3schools.com/jsref/jsref_obj_date.asp)
- [More about bash command: date](https://www.computerhope.com/unix/udate.htm)