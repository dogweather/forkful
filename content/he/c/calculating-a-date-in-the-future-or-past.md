---
title:                "חישוב תאריך בעתיד או בעבר"
html_title:           "C: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## מה ולמה?
חישוב תאריך בעתיד או בעבר הוא תהליך שמאפשר למפתחים לחשב תאריך ספציפי בעתיד או בעבר, בהתאם לצורך. זה יכול להיות שימושי במגוון רחב של תחומי תכנות, כגון יישומי יעוץ פיננסי, לוחות שנה ועוד. 

## איך לעשות:
למפתחים יש כמה דרכים לחשב תאריך בעתיד או בעבר בשפת C. אחת הדרכים הפשוטות היא להשתמש בפונקציות מובנות כמו `mktime()` ו־`localtime()`. ניתן גם להשתמש במודול `time.h` על מנת לגשת לתכונות ולפונקציות נוספות בנושא תאריכים. להלן דוגמאות של קוד ופלט תוצאה עבור חישוב תאריך בעתיד ובעבר:

```C
// תאריך בעתיד
#include <stdio.h>
#include <time.h>

int main()
{
    time_t current_time;
    struct tm *future_tm;
    char output[50];
    
    time(&current_time); // משיכת הזמן הנוכחי
    future_tm = localtime(&current_time);
    // הוספת 7 ימים להזמן הנוכחי
    future_tm->tm_mday += 7;
    mktime(future_tm); // חישוב תאריך חדש
    
    // הדפסת התאריך בפורמט ספציפי
    strftime(output, 50, "תאריך בעתיד: %d/%m/%Y", future_tm);
    printf("%s", output);
    
    return 0;
}
```
פלט התוכנית: 
```
תאריך בעתיד: 19/06/2021
```

```C
// תאריך בעבר
#include <stdio.h>
#include <time.h>

int main()
{
    time_t current_time;
    struct tm *past_tm;
    char output[50];
    
    time(&current_time); // משיכת הזמן הנוכחי
    past_tm = localtime(&current_time);
    // החלפת המשתנה של יום חודש ל־5
    past_tm->tm_mday = 5;
    mktime(past_tm); // חישוב תאריך חדש
    
    // הדפסת התאריך בפורמט ספציפי
    strftime(output, 50, "תאריך בעבר: %d/%m/%Y", past_tm);
    printf("%s", output);
    
    return 0;
}
```
פלט התוכנית:
```
תאריך בעבר: 05/06/2021
```

## חפירה עמוקה:
פיתוח תאריכים הוא תחום עתיק וחשוב בתחום התכנות. בין האפשרויות הנוספות לחישוב תאריכים בעתיד ובעבר ניתן להשתמש בפונקציות ומודולים ייעודיים כמו `difftime()` ו־`timelocal()` שמאפשרים להשוות בין שני תאריכים ולהמיר תאריך ממחרוזת לבנייה של `struct tm`. כדאי לעיין במקורות למידע נוסף על ניצול תאריכים בתוכנות שלכם.

## ראו גם:
- [תיעוד רשמי למודול time.h ב־C](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [פוסט בבלוג על חישוב תאריכים בעתיד ובעבר בקוד C](https://www.geeksforgeeks.org/old-date-todays-date-programming-tricks/)
- [תיעוד לפונקציה mktime ב־C](https://www.cplusplus.com/reference/ctime/mktime/)