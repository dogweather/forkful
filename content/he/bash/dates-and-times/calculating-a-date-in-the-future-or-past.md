---
date: 2024-01-20 17:28:42.633877-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1\u05DE\u05E2\
  \u05E8\u05DB\u05D5\u05EA \u05D9\u05D5\u05E0\u05D9\u05E7\u05E1 \u05D5\u05DC\u05D9\
  \u05E0\u05D5\u05E7\u05E1, \u05D7\u05D9\u05E9\u05D5\u05D1 \u05EA\u05D0\u05E8\u05D9\
  \u05DB\u05D9\u05DD \u05E0\u05E2\u05E9\u05D4 \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD\
  \ \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05E4\u05E7\u05D5\u05D3\u05EA `date`,\
  \ \u05E9\u05D4\u05D5\u05E6\u05D2\u05D4 \u05D1\u05E9\u05E0\u05D5\u05EA \u05D4-70.\
  \ \u05D2\u05D9\u05E9\u05D5\u05EA \u05D0\u05D7\u05E8\u05D5\u05EA \u05DB\u05D5\u05DC\
  \u05DC\u05D5\u05EA \u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05EA\u05D5\u05E1\u05E4\
  \u05D9\u05DD \u05DB\u05DE\u05D5 `dateutils`\u2026"
lastmod: '2024-04-05T22:50:53.770793-06:00'
model: gpt-4-1106-preview
summary: "\u05D1\u05DE\u05E2\u05E8\u05DB\u05D5\u05EA \u05D9\u05D5\u05E0\u05D9\u05E7\
  \u05E1 \u05D5\u05DC\u05D9\u05E0\u05D5\u05E7\u05E1, \u05D7\u05D9\u05E9\u05D5\u05D1\
  \ \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05E0\u05E2\u05E9\u05D4 \u05DC\u05E2\
  \u05D9\u05EA\u05D9\u05DD \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05E4\u05E7\
  \u05D5\u05D3\u05EA `date`, \u05E9\u05D4\u05D5\u05E6\u05D2\u05D4 \u05D1\u05E9\u05E0\
  \u05D5\u05EA \u05D4-70."
title: "\u05D7\u05D9\u05E9\u05D5\u05D1 \u05EA\u05D0\u05E8\u05D9\u05DA \u05D1\u05E2\
  \u05EA\u05D9\u05D3 \u05D0\u05D5 \u05D1\u05E2\u05D1\u05E8"
weight: 26
---

## איך לעשות:
```Bash
# חישוב תאריך 10 ימים מהיום:
date -d "+10 days" '+%Y-%m-%d'

# חישוב תאריך 5 שבועות אחורה:
date -d "-5 weeks" '+%Y-%m-%d'
```
דוגמת פלט:
```Bash
# תאריך 10 ימים מכאן
2023-04-15

# תאריך 35 ימים אחורה
2023-02-15
```

## צלילה עמוקה:
במערכות יוניקס ולינוקס, חישוב תאריכים נעשה לעיתים באמצעות פקודת `date`, שהוצגה בשנות ה-70. גישות אחרות כוללות שימוש בתוספים כמו `dateutils` ובשפות תכנות אחרות.

פקודת `date` משתמשת בפרמטר `-d` להגדרת התאריך, ומאפשרת הזחת תאריך ביחידות מוגדרות כמו ימים ושבועות. ניתן להשתמש גם בחודשים ושנים. הזחת התאריך מחושבת בהתאם לפרקי זמן מדויקים, ולכן יש להיות מודעים לשינויים כמו שנים מעוברות וקפיצת שעה קיצומית.

## ראו גם:
- [GNU Coreutils - Date documentation](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html): מסמך מדריך לפקודת `date`.
- [Dateutils on GitHub](https://github.com/hroptatyr/dateutils): סט כלים לחישובים עם תאריכים.
- [Bash scripting cheatsheet](https://devhints.io/bash): רשימת פקודות שימושיות לתכנות ב-Bash.
