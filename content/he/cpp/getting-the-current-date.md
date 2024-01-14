---
title:    "C++: קבלת תאריך נוכחי"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## למה

 בכל תוכנית שנכתוב בסיס לשפת תכנות סי פלוס פלוס (C++) ישנו צורך לקבל את התאריך הנוכחי. אם נרצה להיות דייקים ניתן לשתותש בפקודות תאריך חיצוניות, אבל ישנן פתרונות נוספים שבעיקר ישמשו אנשי תוכנה או מפתחי תוכנה.

## כיצד לקבל את התאריך הנוכחי

כדי לקבל את התאריך הנוכחי בשפת תכנות C++, נשתמש בפונקציה `gettimeofday()` מספרת `sys/time.h`. התחביר הכללי של הפונקציה הוא:

```C++
#include <sys/time.h> 

int gettimeofday(struct timeval *tv, struct timezone *tz);
```

עם זאת, מימושי המערכת שונים יעשו שימוש בפונקציות אחרות, אז מומלץ לבדוק את המימוש של המערכת על מנת להבין אילו פונקציות זמינות ואילו פונקציות הם עיקריות.

הנה דוגמא פשוטה לקבלת התאריך הנוכחי ושימוש בו:

```C++
#include <iostream>
#include <ctime> 

int main()
{
    // create a struct to hold the time
    time_t rawtime;
    // get current time and store it in the struct
    time (&rawtime);
    // print the current time
    cout << "The current date and time is: " << ctime(&rawtime) << endl;
    return 0;
}
```

פלט יכול להיות משהו כזה:

```
The current date and time is: Mon Jul  5 15:53:25 2021
```

בנוסף לזה, ישנן גם ספריות פופולריות כמו ה-Boost Date Time שמציעה פתרונות מתקדמים יותר עבור קבלת התאריך הנוכחי. ככל שהמערכת גדולה יותר ומורכבת יותר, כך יש לו גם פתרונות יותר.

## חפירה עמוקה

אם נרצה להבין איך `gettimeofday()` מטפלת בפעולה זו, כדאי לברר את הקובץ `sys/time.h` בחבילת הנתונים של C++. הפונקציה מטפלת בעיתות זמן כדי לקבל את התאריך הנוכחי בנ