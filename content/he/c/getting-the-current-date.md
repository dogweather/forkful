---
title:                "קבלת התאריך הנוכחי"
html_title:           "C#: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## मा ולמה?
משום שזה מאפשר למתכנתים לשלוט ולשמש את התאריך והשעה הנוכחיים, זהו אלמנט הכרחי בפונקציונליות מערכת ההפעלה. אתה יכול להשתמש בזה לבצע לוגים, השוואות זמן, או ספירות מדוברות ביישומים שונים.

## איך משיגים:
להלן דוגמאות קוד ופלטי דגימה בקודי C:
```C
#include <time.h>
#include <stdio.h>

int main() {
    time_t current_time;
    char* c_time_string;

    /* Obtain current time. */
    current_time = time(NULL);

    /* Convert to local time format. */
    c_time_string = ctime(&current_time);

    /* Print to stdout. */
    printf("Current time is %s", c_time_string);
    return 0;
}
```
פלט מנעד זה יכול להיות משהו בסגנון:
```
Current time is Wed Jun 2 19:19:31 2021
```
## Deep Dive
1. **הקשר היסטורי:** תיאור הזמן הנוכחי באמצע של מערכת ההפעלה הוא מרכזי להגדרה של ה"תקן C", תקן מתכנת המחשב שהעיד בעשור של שימוש שוטף, התנסות, וחידוש.
2. **אלטרנטיבות:** ב- C++, אפשר להשתמש ב- `std::chrono` לקבלת הזמן הנוכחי. ב- Python, ניתן להשתמש במודול הספרייה `datetime`.
3. **פרטי הגשמה:** הפונקציה `time` מחזירה את הזמן הנוכחי מאז שנת 1970 (epoch), והפונקציה `ctime` ממירה את הזמן הזה לפורמט מחרוזת אנושי.

## ראו גם
1. [תיעוד הפונקציה `time`](http://www.cplusplus.com/reference/ctime/time/)
2. [תיעוד הפונקציה `ctime`](http://www.cplusplus.com/reference/ctime/ctime/)
3. [היסטורית תקן C](https://en.wikipedia.org/wiki/C_%28programming_language%29#History)
4. ['epoch'](https://en.wikipedia.org/wiki/Unix_time) של Unix.