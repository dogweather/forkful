---
title:                "חישוב תאריך בעתיד או בעבר"
date:                  2024-01-20T17:31:12.324742-07:00
model:                 gpt-4-1106-preview
simple_title:         "חישוב תאריך בעתיד או בעבר"

category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## מה ולמה?
חישוב תאריך בעתיד או בעבר הוא תהליך שבו אנו מוסיפים או מחסירים ימים, שבועות, חודשים, או שנים מתאריך נתון. תכנתים עושים זאת לתכנון עתידי, הזמנות, לוחות זמנים, ותזכורות.

## איך לעשות:
```c
#include <stdio.h>
#include <time.h>

// חישוב והדפסת תאריך חמישה ימים מהיום
int main() {
    time_t now;
    time(&now);
    struct tm newDate = *localtime(&now);

    // הוספת חמישה ימים לתאריך הנוכחי
    newDate.tm_mday += 5;
    mktime(&newDate);

    // הדפסת התאריך החדש
    char buffer[80];
    strftime(buffer, 80, "%d-%m-%Y", &newDate);
    printf("תאריך חמישה ימים מעכשיו: %s\n", buffer);

    return 0;
}
```
פלט לדוגמא:
```
תאריך חמישה ימים מעכשיו: 30-03-2023
```

## צלילה עמוקה
חישוב תאריך הוא בעל היסטוריה ארוכה, מימי השעון השמש ועד עידן ה-Kernel של מערכות ההפעלה המודרניות. חלופות כוללות שימוש בפונקציות שונות מהספרייה הרגילה של C, או שימוש בספריות חיצוניות כמו `date.h`. כאשר אנו חושבים על חישוב תאריך, צריך להיזהר משגיאות נפוצות כמו זיעוז בין זמני קיץ וחורף, עיבוד שגוי של חודשים עם מעט ימים מהרגיל, והתעלמות משנים מעוברות. ניתן להשתמש ב-funcion `mktime` כדי לוודא שהתאריך יושבץ נכון לאחר שינויים.

## ראה גם
- [C Time Library](https://www.cplusplus.com/reference/ctime/)
- [Stack Overflow - How to add days to `struct tm`?](https://stackoverflow.com/questions/1442116/how-to-get-the-date-and-time-values-in-a-c-program)
