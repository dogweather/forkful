---
aliases:
- /he/fish-shell/calculating-a-date-in-the-future-or-past/
date: 2024-01-20 17:31:03.920516-07:00
description: "\u05D7\u05D9\u05E9\u05D5\u05D1 \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD\
  \ \u05D1\u05E2\u05D1\u05E8 \u05D0\u05D5 \u05D4\u05E2\u05EA\u05D9\u05D3 \u05D4\u05D5\
  \u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05DC \u05D4\u05D5\u05E1\u05E4\u05D4\
  \ \u05D0\u05D5 \u05D4\u05D7\u05E1\u05E8\u05D4 \u05E9\u05DC \u05D9\u05DE\u05D9\u05DD\
  \ \u05D0\u05D5 \u05D7\u05D5\u05D3\u05E9\u05D9\u05DD \u05DE\u05EA\u05D0\u05E8\u05D9\
  \u05DA \u05DE\u05E1\u05D5\u05D9\u05DD. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\
  \u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DC\u05EA\u05D9\u05D0\
  \u05D5\u05DD \u05DE\u05E9\u05D9\u05DE\u05D5\u05EA, \u05EA\u05D6\u05DE\u05D5\u05E0\
  \u05D9\u05DD \u05D5\u05DC\u05D2\u05D6\u05D5\u05E8 \u05DE\u05E1\u05E7\u05E0\u05D5\
  \u05EA \u05DE\u05DE\u05E8\u05D5\u05D5\u05D7\u05D9\u2026"
lastmod: 2024-02-18 23:08:53.309512
model: gpt-4-1106-preview
summary: "\u05D7\u05D9\u05E9\u05D5\u05D1 \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD\
  \ \u05D1\u05E2\u05D1\u05E8 \u05D0\u05D5 \u05D4\u05E2\u05EA\u05D9\u05D3 \u05D4\u05D5\
  \u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05DC \u05D4\u05D5\u05E1\u05E4\u05D4\
  \ \u05D0\u05D5 \u05D4\u05D7\u05E1\u05E8\u05D4 \u05E9\u05DC \u05D9\u05DE\u05D9\u05DD\
  \ \u05D0\u05D5 \u05D7\u05D5\u05D3\u05E9\u05D9\u05DD \u05DE\u05EA\u05D0\u05E8\u05D9\
  \u05DA \u05DE\u05E1\u05D5\u05D9\u05DD. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\
  \u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DC\u05EA\u05D9\u05D0\
  \u05D5\u05DD \u05DE\u05E9\u05D9\u05DE\u05D5\u05EA, \u05EA\u05D6\u05DE\u05D5\u05E0\
  \u05D9\u05DD \u05D5\u05DC\u05D2\u05D6\u05D5\u05E8 \u05DE\u05E1\u05E7\u05E0\u05D5\
  \u05EA \u05DE\u05DE\u05E8\u05D5\u05D5\u05D7\u05D9\u2026"
title: "\u05D7\u05D9\u05E9\u05D5\u05D1 \u05EA\u05D0\u05E8\u05D9\u05DA \u05D1\u05E2\
  \u05EA\u05D9\u05D3 \u05D0\u05D5 \u05D1\u05E2\u05D1\u05E8"
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
