---
title:                "המרת תאריך למחרוזת"
html_title:           "Bash: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
המרת תאריך למחרוזת היא הליך שבו ממירים את משתמע משתמע של תאריך מסוג של הכלים למחרוזת של תווים. מתכנתים משתמשים בתהליך זה כדי להציג תאריכים בצורה מוכרת למשתמשים או לשמור על בסיס נתונים בצורה תקנית.

## איך לעשות:

ניתן להמיר תאריך למחרוזת באמצעות פקודה `date` של Fish Shell:

```Fish Shell
set date_string (date -u "+%Y-%m-%d_%H:%M:%S")
echo $date_string
```
פלט דוגמה:

```Fish Shell
2022-03-12_13:45:00
```

## השקעה עמוקה:

ההמרה של תאריך למחרוזת היא מנגנון שמשמש מאז השנים הראשונות של המחשוב, מאז שהחומרה התאמה תאריכים ושעות למערכות. בספרית C הסטנדרטית ישנן פונקציות מובנות למרת תאריך למחרוזת, ההמרה מתבצעת פנימית במערכת ההפעלה. כמו כן, בסביבות שפות תכנות אחרות ישנן ספריות עם יכולות דומות.

## ראה גם:

* [ציוות השפה של Fish](https://fishshell.com/)
* [מדריך למשתמש של Fish](https://fishshell.com/docs/current/index.html)
* [חלופות לפקודת date של Fish Shell](https://fishshell.com/docs/current/cmds/date.html)