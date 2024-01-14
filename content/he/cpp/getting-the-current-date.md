---
title:    "C++: לקבלת התאריך הנוכחי"
keywords: ["C++"]
---

{{< edit_this_page >}}

## למה

פריסת תוכניות מרובות תלויה באופן שבו הן מתגלגלות ומאגדות נתונים, בה במקרה שלנו היא תאריך נוכחי. באמצעות תאריך נוכחי, התוכנה יכולה להפגין תנועה ולשרוך אירועים אליו.

## איך לעשות זאת

בתכנות C++ ישנם מספר דרכים לקבלת תאריך נוכחי. הדרך הנפוצה ביותר היא על ידי שימוש בפונקציית `time()` והפעלתה על תאריך נוכחי. לדוגמה:

```C++
#include <iostream>
#include <ctime>

int main() {
    // משתנה לאחסון זמן נוכחי
    time_t now = time(0);

    // הפעלת התאריך הנוכחי על פני משתנה מטיפוס מספרי
    tm* currentDate = localtime(&now);

    // הצגת תאריך נוכחי על ידי שימוש בפונקציות בנויות
    std::cout << "התאריך הנוכחי הוא: " << (currentDate->tm_mon + 1) << "/" << currentDate->tm_mday << "/" << (currentDate->tm_year + 1900) << std::endl;

    return 0;
}
```

פלט ישלח:

```
התאריך הנוכחי הוא: 12/16/2021
```

## טיפול עמוק

בנוסף לשיטה המוצגת בדוגמה למעלה, ניתן להשתמש בספריות נוספות כמו `chrono` ו-`ctime` לעבודה עם תאריך נוכחי. כמו כן, ניתן להשתמש בפונקציות נוספות כגון `gettimeofday()` ו-`localtime_s()`. בקישורים מתחת תוכלו למצוא מידע נוסף על כל אחת מהשיטות.

## ראו גם

- [תיעוד ctime](https://www.cplusplus.com/reference/ctime/)
- [תיעוד chrono](https://www.cplusplus.com/reference/chrono/)
- [מדריך על תאריך נוכחי ב-C++](https://www.geeksforgeeks.org/current-date-and-time-using-c-programming/)
- [תיעוד gettimeofday()](https://linux.die.net/man/2/gettimeofday)
- [מדריך לתאריך נוכחי בשפת C++](https://www.guru99.com/c-date-time.html)