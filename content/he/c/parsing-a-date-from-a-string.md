---
title:                "ניתוח תאריך ממחרוזת"
date:                  2024-01-20T15:35:33.034459-07:00
html_title:           "Arduino: ניתוח תאריך ממחרוזת"
simple_title:         "ניתוח תאריך ממחרוזת"

category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
Parsing a date from a string בקיצור זה לקחת מידע על תאריך מתוך טקסט. מתכנתים עושים את זה כדי לנתח קלט של תאריך, לשמור נתונים, להציג אותם או לבצע חישובים.

## איך לעשות:
```c
#include <stdio.h>
#include <time.h>

int main() {
    struct tm tm;
    char *date_str = "2023-03-15"; // דוגמה עבור תאריך
    if (strptime(date_str, "%Y-%m-%d", &tm)) {
        printf("Year: %d, Month: %d, Day: %d\n", tm.tm_year + 1900, tm.tm_mon + 1, tm.tm_mday);
    } else {
        printf("Parsing failed!\n");
    }
    return 0;
}
```
פלט דוגמה:
```
Year: 2023, Month: 03, Day: 15
```

## עיון מעמיק:
Parsing dates from strings היה חלק מליבה של תיכנות כבר מאז הימים שבהם מחשבים התחילו לטפל בנתונים. יש כלים רבים לעשות את זה, כמו `strptime` ב-C ופונקציות בשפות נוספות. בחירת הפונקציה תלויה במערכת, בהעדפות האישיות ובפורמט התאריך. לפני `strptime`, היו התקנים שונים לפרסור תאריכים, ואין פורמט סטנדרטי אחד שכולם עובדים איתו.

## ראו גם:
- תיעוד `strptime` ב-man pages: https://man7.org/linux/man-pages/man3/strptime.3.html
- מדריך לפונקציות תאריך וזמן ב-C: https://www.tutorialspoint.com/c_standard_library/c_function_strptime.htm
- עוד על פורמט תאריך וזמן ISO 8601 (הפורמט שנמצא בדוגמה): https://en.wikipedia.org/wiki/ISO_8601
