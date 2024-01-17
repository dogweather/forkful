---
title:                "קבלת התאריך הנוכחי"
html_title:           "C++: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?
קבלת התאריך הנוכחי היא פעולה נפוצה בתכנות המאפשרת למתכנתים לקבל תאריך מדויק ועדכני במחשבם. מתכנתים משתמשים בפעולה זו כדי לוודא תאריך ושעה מדויקים, להדגיש זמנים חשובים בתוכניות ולבצע פעולות מבוססות תאריך כגון יישומים למנהלי פקודות.

## איך לעשות זאת?
הנה כמה דוגמאות לקבלת התאריך הנוכחי בשפת C++ והפלט המייצג:

```C++
#include <iostream>
#include <ctime>
using namespace std;

int main ()
{
  // הגדרת משתנה כדי לאחסן את התאריך הנוכחי
  time_t now = time(0);
  
  // הצגת התאריך הנוכחי בפורמט מסוים
  cout << "התאריך הנוכחי: " << ctime(&now);
  
  // הצגת השנה הנוכחית מהתאריך
  cout << "השנה הנוכחית: " << localtime(&now)->tm_year + 1900 << endl;
  
  return 0;
}

// פלט:
// התאריך הנוכחי: Thu Dec 16 20:19:02 2021
// השנה הנוכחית: 2021
```

## הוטבע?
קבלת התאריך הנוכחי בתכנות מתבססת על כימות זמן שנקראת תאריך אבן. התאריך הנוכחי מתקבל ממאגר נתונים המכיל תאריכים המתווכים בין התאריך הראשון לתאריך הנוכחי ביחס לכמות השניים שחלפו מאז.

כמו כן, ישנן אפשרויות נוספות לקבלת התאריך הנוכחי, כגון שימוש בפקודות במערכת ההפעלה או בשימוש בספריות חיצוניות המספקות פעולות קבלת תאריך.

## ראי גם
לפניכם כמה מקורות נוספים הקשורים לקבלת התאריך הנוכחי בשפת C++:

- [C++ Reference: `time`](https://www.cplusplus.com/reference/ctime/time/)
- [GeeksforGeeks: Getting Current Date and Time in C++](https://www.geeksforgeeks.org/get-current-date-and-time-in-cpp/)
- [Stack Overflow: C++ current date and time](https://stackoverflow.com/questions/997946/how-to-get-current-time-and-date-in-c)