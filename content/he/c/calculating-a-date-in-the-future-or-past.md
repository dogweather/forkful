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

## למה
המחשבתך תצטרך לחשב תאריכים בעתיד או בעבר משמשת בדרך כלל ליצירת לוחות זמנים או לתזמון אירועים. אם נחשב שיש תכניות לשינוי של מועד או להזזתו, יכולה להיות שעדיף לתכנן בשנים הבאות. 

## כיצד לעשות זאת
הבנה של החישובים הנכונים תלויה בקידומת של חומרה והחלטות כלליות אחרות כגון האיישור של ארגומנטים שונים לפונקציה. כדי לחשב תאריכים בעתיד או בעבר, עלינו להשתמש בפונקציות כמו `time.h` ו `locale.h`. הפונקציה `time` מציגה את תאריך ההחשבה הנוכחית הרצויה ו `mktime` ממירה את התאריך לערך בפורמט של פקודת הסיפור. לדוגמה:

```
#include <stdio.h>
#include <time.h>

int main() {
    time_t now = time(NULL);
    printf("Time since epoch: %ld\n", now);
    struct tm target;
    strptime("2027 13;20;05", "%Y %H:%M:%S", &target);
    // Buffer for strptime.
    printf("Date: %s", asctime(&target));
    printf("Target: %ld\n", (long) mktime(&target));
    
    return 0;
}
```

### תוצאה:
```
Time since epoch: 1612180218
Date: Mon Jan  1 13:20:05 2027
Target: 1745294405
```

## לקרוא עוד
- [תיעוד הנדל"ת C Function](https://www.codingame.com/playgrounds/14213/how-to-play-with-dates-and-times-in-c/2-calculations-using-c-functions)
- [מראה נשלט באתר שימוש סיכום תאריך בשפת C](https://www.guru99.com/c-date-time.html)
- [האינטרנט הבישוי לשפת C מכיל בצורת הדפדפן לקרוא עוד](https://www.tutorialspoint.com/c_standard_library/time_h.htm)