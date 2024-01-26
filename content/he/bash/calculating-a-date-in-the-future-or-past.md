---
title:                "חישוב תאריך בעתיד או בעבר"
date:                  2024-01-20T17:28:42.633877-07:00
model:                 gpt-4-1106-preview
html_title:           "Bash: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## מה ולמה?
חישוב תאריך בעתיד או בעבר מתייחס לשינוי של תאריך נתון במספר ימים, שבועות, חודשים או שנים. מתכנתים עושים זאת לתיאום לוחות זמנים, תזכורות, ולתכנון קדימה או התאוששות אחורה.

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
