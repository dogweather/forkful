---
title:                "השוואה בין שני תאריכים"
html_title:           "Arduino: השוואה בין שני תאריכים"
simple_title:         "השוואה בין שני תאריכים"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מה ולמה?

השוואת שניי תאריכים היא תהליך בו אנו קובעים איזה תאריך הגיע לפני השני, או אם הם זהים. זה מסייע למתכנתים לסדר אירועים בהיסטוריה שבהם הם מתרחשים, לקבוע טווחים של זמן ועוד.

## איך לעשות:

‏```C
#include<stdio.h>
#include<time.h>
int main() {
    // Initializing two different time structures
    struct tm time1 = {0}, time2 = {0};
    time1.tm_year = 100;  // Years since 1900
    time1.tm_mon = 0;    // Months since January - [0,11]
    time1.tm_mday = 1;   // Day of the month - [1,31] 
    time2.tm_year = 100;
    time2.tm_mon = 0;
    time2.tm_mday = 2;
    // Comparing the two dates
    double difference = difftime(mktime(&time2), mktime(&time1));
    printf("Difference is: %.f seconds\n", difference);
    return 0;
}
```
הפלט הצפוי:
‏```C
Difference is: 86400 seconds
```
## שיעור מעמיק:

‏**ההקשר ההיסטורי:** ראשית, בשפת C, היינו צריכים להמיר כל תאריך לפורמט אחר כדי להשוות בין תאריכים. את זאת השגנו באמצעות הפונקציה difftime המהודרת מהספרייה time.h.
‏**החלופות:** אפשרויות חלופות להשוואת תאריכים כוללות שימוש בשפות אחרות, שימוש בספריות של צד שלישי, או כתיבת קוד משלך לביצוע ההשוואה.
‏**פרטי היישום:** כאשר אנו משווים תאריכים, אנו ממירים כל תאריך לשניות שחלפו מ-UNIX Epoch, ואז מחזירים את ההפרש. ה-UNIX Epoch הוא תאריך שנקבע ל-1 בינואר 1970.

## ראו גם:

- [שפע של ספריות להשוואת תאריכים ב-C](https://www.cplusplus.com/reference/ctime/)
- [מדריך מקיף על ה-UNIX Epoch](https://en.wikipedia.org/wiki/Unix_time)
- [הסבר על הפונקציה difftime](https://www.tutorialspoint.com/c_standard_library/c_function_difftime.htm)