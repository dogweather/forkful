---
title:                "קבלת תאריך נוכחי"
html_title:           "Fish Shell: קבלת תאריך נוכחי"
simple_title:         "קבלת תאריך נוכחי"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## למה
אנשים מתעסקים עם קבלת התאריך הנוכחי מסיבות רבות, כגון יצירת תוויות זמן בקבצי תוכנית או בתיעוד, או עדכון יום בברכה בסקריפטים רצים.

## איך לעשות זאת
ניתן לקבל את התאריך הנוכחי באמצעות פקודת `date` שמובנית בתוך מסך השיח של Fish Shell. באמצעות הפרמטרים המתאימים, ניתן לקבל את התאריך בפורמט שנרצה. לדוגמה:

```
Fish Shell> date +%A, %B %d, %Y
רביעי, אוקטובר 06, 2021
```

הפרמטר `%A` מציג את שם היום בשבוע, `%B` מציג את שם החודש ו `%d` מציג את התאריך הנוכחי בחודש. ניתן להוסיף גם פרמטרים נוספים כגון `%H` להצגת שעה נוכחית ו `%M` להצגת דקות נוכחיות.

## טיפול מעמיק
לקבלת מידע נוסף על דרכי התאריך הנוכחי, ניתן לעיין בתיעוד המלא של פקודת `date` במסך השיח של Fish Shell. ניתן לגשת אליו על ידי הרצת הפקודה `man date`.

## ראה גם
- [מסך השיח של Fish Shell](https://fishshell.com/docs/current/index.html)
- [תיעוד לפקודת `date`](https://fishshell.com/docs/current/commands.html#date)