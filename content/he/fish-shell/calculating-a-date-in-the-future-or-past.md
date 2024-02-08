---
title:                "חישוב תאריך בעתיד או בעבר"
aliases:
- he/fish-shell/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:31:03.920516-07:00
model:                 gpt-4-1106-preview
simple_title:         "חישוב תאריך בעתיד או בעבר"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## מה ולמה?
חישוב תאריכים בעבר או העתיד הוא פעולה של הוספה או החסרה של ימים או חודשים מתאריך מסוים. תכניתנים עושים זאת לתיאום משימות, תזמונים ולגזור מסקנות ממרווחי זמן.

## איך לעשות:
הנה דוגמאות לחישוב תאריכים ב-Fish Shell, פשוטות וישירות:

לחשב תאריך עוד 10 ימים:

```Fish Shell
set date_future (date -d "+10 days" "+%Y-%m-%d")
echo $date_future
```
פלט דוגמא:
```
2023-05-10
```

לחשב תאריך לפני 5 ימים:

```Fish Shell
set date_past (date -d "-5 days" "+%Y-%m-%d")
echo $date_past
```
פלט דוגמא:
```
2023-04-25
```

## עיון מעמיק
שימוש בפונקציית `date` ב-shell נפוץ בקרב תכניתנים לחישוב זמנים מוחלטים ויחסיים. ההיסטוריה של פקודה זו מתחילה רחוק, עם התפתחות מערכות Unix. ישנם חלופות כמו GNU `date` ב-Linux, ו-`BSD date` ב-macOS. פרטי היישום כוללים שימוש ב-flags כמו `-d` לציון ההפרש הזמני מהתאריך הנוכחי והפורמט `-I` או "`+%Y-%m-%d`" כדי לפלט את התאריך בפורמט סטנדרטי.

## ראה גם
- תיעוד של Fish Shell על עבודה עם תאריכים: https://fishshell.com/docs/current/index.html#expand-date
- GNU coreutils `date` מדריך: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- `BSD date` מדריך למשתמש: https://www.freebsd.org/cgi/man.cgi?query=date
