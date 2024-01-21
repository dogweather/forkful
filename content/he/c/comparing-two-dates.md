---
title:                "השוואת שתי תאריכים"
date:                  2024-01-20T17:33:00.388789-07:00
model:                 gpt-4-1106-preview
simple_title:         "השוואת שתי תאריכים"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מה ולמה?
כאשר מדובר בהשוואת תאריכים, מתכנתים בודקים מי מבין שני תאריכים קדם לשני או אם שניהם זהים. זוהי פעולה חשובה בתכנות, ניוד נתונים, ובדיקת חוקיות כדי לקבוע תקופות זמן, עדכניות תוכן או פג תוקף.

## איך לעשות:
ב-C, תאריכים יכולים להישוות על ידי המרה למספרים והשוואה ביניהם. להלן קוד לדוגמה:

```C
#include <stdio.h>
#include <time.h>

int compare_dates(struct tm tm1, struct tm tm2) {
    time_t t1 = mktime(&tm1);
    time_t t2 = mktime(&tm2);

    if (t1 < t2) 
        return -1; // tm1 is earlier
    else if (t1 > t2) 
        return 1; // tm1 is later

    return 0; // dates are equal
}

int main() {
    struct tm date1 = { .tm_year=122, .tm_mon=3, .tm_mday=15 }; // 15 April 2022
    struct tm date2 = { .tm_year=122, .tm_mon=3, .tm_mday=20 }; // 20 April 2022

    int result = compare_dates(date1, date2);
    if (result == -1)
        printf("Date1 is earlier than Date2\n");
    else if (result == 1)
        printf("Date1 is later than Date2\n");
    else
        printf("Date1 is the same as Date2\n");

    return 0;
}
```

פלט דוגמה:
```
Date1 is earlier than Date2
```

## עיון מעמיק:
להשוואת תאריכים יש מסורת ארוכה במדעי המחשב. לדוגמה, תקינה של נתונים היא שימוש נפוץ להשוואה זו. לשם כך, משתמשים בפונקציות של `time.h`, חבילת ספריות סטנדרטית ב-C המספקת מנגנוני זמן ותאריך. בפועל, מתכנתים נעזרים במבנה `struct tm`, שמייצג תאריך ושעה, ובפונקציית `mktime` להמרת המבנה ל`time_t` שהינו סוג משתנה המייצג שניות שחלפו מאז ה-1 בינואר, 1970 (תקן ה-UNIX Epoch). השוואה של שני אובייקטים מסוג `time_t` חד-משמעית ופשוטה.

כמו כן, קיימים גישות חלופיות כמו ספריות צד שלישי ו-APIs כמו `Boost` ב++C או `DateTime` ב.NET, אשר מציעות חווית משתמש משופרת ותכונות נוספות להשוואת תאריכים וניהול זמנים מורכבים יותר.

## ראו גם:
- [C Standard Library - time.h](https://en.cppreference.com/w/c/chrono)