---
title:    "Fish Shell: המרת תאריך למחרוזת"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## למה

כדי לעשות שימוש בתוכנית כדי להציג את התאריך כמחרוזת. 

## כיצד לעשות זאת

כדי להמיר תאריך למחרוזת עם שלט הפרקוד הפוך, ניתן להשתמש בפקודה `date`. לדוגמה, אם נרצה להציג את התאריך הנוכחי בתאריך בפורמט של "יום-חודש-שנה", ניתן להשתמש בפקודה הבאה:

```Fish Shell
date "+%d-%m-%Y"
```

כתוצאה מכך, התוכנית תציג את התאריך הנוכחי בפורמט הרצוי, לדוגמה 01-10-2021. 

## העמקה

להמרת תאריך למחרוזת ישנן תוספות נוספות שניתן להשתמש בהן עם הפקודה `date`. לדוגמ התוספ מתאר את התאריך בפורמט של חציון, ניתן להשתמש בפקודה הבאה:

```Fish Shell
date "+%A %B %e, %Y"
```

כתוצאה מכך, התוכנית תציג את התאריך בפורמט של יום, חודש, ויום החודש הנוכחי, לדוגמה "יום חמישי אוקטובר 1, 2021".

## ראה גם

למידע נוסף על השתמשת בפקודת `date`, ניתן לבקר בלינקים הבאים:

- [תיעוד עבור פקודת `date` (כדי להמיר תאריך למחרוזת)](https://fishshell.com/docs/current/cmds/date.html)
- [פיתוח כבלים בפקודת `date`](https://fishshell.com/docs/current/cmds/date.html#development-eksamples)
- [טיפים לשימוש בתירוץ הפקודה `date`](https://fishshell.com/docs/current/cmds/date.html#useful-examples)

תהנו מהשימוש בפקודת `date` כדי להמיר תאריך למחרוזת בפשטות!