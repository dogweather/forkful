---
title:                "פענוח תאריך ממחרוזת"
html_title:           "Bash: פענוח תאריך ממחרוזת"
simple_title:         "פענוח תאריך ממחרוזת"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
פרסונג של תאריך ממחרוזת הוא התהליך שבו אנחנו ממירים מחרוזת המכילה תאריך לממד נתונים שאנו יכולים לעבוד איתו, כמו struct. מתכנתים עושים את זה כדי לנהל מידע שהוזן דרך ממשק משתמש או שנאמץ ממקור חיצוני.

## איך:
```C 
#include <time.h>
#include <stdio.h>
#include <stdlib.h>

int main()
{
    struct tm tm;
    char buf[255];

    memset(&tm, 0, sizeof(struct tm));
    strptime("2001-11-12 18:31:01", "%Y-%m-%d %H:%M:%S", &tm);
    strftime(buf, sizeof(buf), "%d %b %Y %H:%M", &tm);
    puts(buf);

    exit(EXIT_SUCCESS);
}
```

הפלט המנוהל מייצג את התאריך והשעה שבה המיון מתבצע, בפורמט שאנו בחרנו:
```
12 Nov 2001 18:31
```

## הצלילה העמוקה:
פרסונג של מחרוזת לתאריך הוא דרך ראשית לביצוע המרה של דרך התאריך מנתונים שנקלטו בפורמט שאינו מסודר. עם זאת, בהינתן שתחביר התאריכים עשוי להשתנות בין משתמשים, מערכות ושפות, נדרשת גמישות.

ישנם דרכים חלופיות לפרסונג תאריך, כמו כאלה המבוססות על ביטויים רגילים או שימוש בספריות ספציפיות.

## ראה גם:
- [תיעוד הפונקציית strptime ב- GNU](https://www.gnu.org/software/libc/manual/html_node/Date_002dInput-Functions.html)
- [תיעוד strptime ב- POSIX](https://pubs.opengroup.org/onlinepubs/009695399/functions/strptime.html)
- [ISO 8601 - תקן זמן ותאריך](https://www.iso.org/iso-8601-date-and-time-format.html)