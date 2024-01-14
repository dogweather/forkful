---
title:    "C: חישוב תאריך בעתיד או בעבר"
keywords: ["C"]
---

{{< edit_this_page >}}

## למה
מחשבת לתוכנן ולחשב תאריכים בעתיד או בעבר עשויה להיות מורכבת ומאתגרת, אבל היא יכולה להיות מאוד שימושית בשביל מטרות שונות. למשל, ניתן להשתמש בחישובי תאריכים כדי לתכנן את השימוש ביחסי הזמן באפליקציות או ליצור ולנהל לוח זמנים.

## איך לעשות זאת
קוד C מכיל מספר כלים חזקים כדי לנהל תאריכים. ניתן להשתמש בפונקציות כמו mktime כדי להמיר תאריכים ולדעת את היום של השבוע, או ניתן לבנות פונקציות משלנו שתבצע את החישוב לפי הצורך. הנה דוגמאות קוד בשפת C עם פלט דוגמה:

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    // Getting today's date
    time_t today = time(NULL);
    printf("Today's date is: %s\n", ctime(&today));

    // Calculating one month from today
    struct tm *future_date;
    future_date = localtime(&today);
    future_date->tm_mon += 1;
    mktime(future_date);
    printf("One month from today will be: %s\n", asctime(future_date));

    // Calculating one year and two months ago
    struct tm *past_date;
    past_date = localtime(&today);
    past_date->tm_year -= 1;
    past_date->tm_mon -= 2;
    mktime(past_date);
    printf("One year and two months ago was: %s\n", asctime(past_date));

    return 0;
}
```

```
Output:
Today's date is: Sun Aug 01 2021

One month from today will be: Wed Sep 01 2021

One year and two months ago was: Fri May 01 2020
```

## חקירה מעמיקה
חישובי תאריכים יכולים להיות מאוד מורכבים והם עשויים להעלות אתגרים בעתידות כמו איחודי קיץ או חודשי ספטמבר. אבל עם הידע המתאים והכלים הנכונים, ניתן לנהל בקלות תאריכים בשפת C כדי להתמודד עם כל משימה כזו.

למידע נוסף ולדוגמאות נוספות על חישובי תאריכים בשפת C, אנו ממליצים לשים עין על הקישורים הבאים:

- [מדריך לפונקציות ת