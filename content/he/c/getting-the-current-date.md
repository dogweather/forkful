---
title:                "קבלת התאריך הנוכחי"
date:                  2024-01-20T15:13:18.481091-07:00
html_title:           "C: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה? (What & Why?)
לקבל את התאריך הנוכחי ב-C זה כמו להגיד למחשב "מה השעה?". תכניתנים עושים את זה כי לפעמים תאריכים הם חלק מהמשחק - חותמות זמן, תיעוד, והתאמות.

## איך לעשות: (How to:)
הנה קוד פשוט שמציג את התאריך והשעה הנוכחיים:

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t now;
    time(&now); // קבל את הזמן הנוכחי

    struct tm *local = localtime(&now); // המיר לזמן מקומי

    printf("התאריך והשעה הנוכחיים: %02d/%02d/%d %02d:%02d:%02d\n",
           local->tm_mday, local->tm_mon + 1, local->tm_year + 1900,
           local->tm_hour, local->tm_min, local->tm_sec);

    return 0;
}
```

שימו לב, הפלט יראה כך (עם התאריך והשעה בזמן הריצה):
```
התאריך והשעה הנוכחיים: 12/04/2023 15:41:30
```

## להבין את העומק: (Deep Dive)
בימים הראשונים של התכנות ב-C, לא היה סטנדרט אחיד לניהול תאריכים. הספרייה `<time.h>` שמוספה ב-ANSI C תיקנה את זה. יש גם אלטרנטיבות כמו ה-Funׁׁction strftime() שנותנת יותר גמישות בתבניות פלט. כל זה עובד בזכות מערכת המחשב ששומרת חותמות זמן מהידוע כ-'Epoch' (1 בינואר 1970).

## ראה גם: (See Also)
- תיעוד [`<time.h>`](http://en.cppreference.com/w/c/chrono) ב-CPP Reference.
- [`strftime`](http://en.cppreference.com/w/c/chrono/strftime) לפורמטים מורכבים יותר של זמן ותאריך.
- קורס מקוון ל-C שכולל יחידה על ניהול זמן ותאריכים.
