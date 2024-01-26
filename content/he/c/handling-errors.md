---
title:                "ניהול שגיאות"
date:                  2024-01-26T00:37:12.204794-07:00
model:                 gpt-4-1106-preview
simple_title:         "ניהול שגיאות"
programming_language: "C"
category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/handling-errors.md"
---

{{< edit_this_page >}}

## מה ולמה?
טיפול בשגיאות ב-C זה כמו לצפות לצפוי. זה מונע מתוכניות להשתגע כשהן נתקלות בבעיות. מתכנתים עושים זאת כדי לטפל בטעויות באופן רחמני ולשמור על האמינות של הקוד שלהם.

## איך לעשות:

בואו נראה איך לעשות את זה ב-C:

```C
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

int main() {
    FILE *fp = fopen("nonexistentfile.txt", "r");
    if (fp == NULL) {
        perror("שגיאה בפתיחת הקובץ");
        return EXIT_FAILURE;
    }
    // לעשות משהו עם הקובץ
    fclose(fp);
    return EXIT_SUCCESS;
}
```

דוגמא לפלט כשהקובץ לא קיים:
```
שגיאה בפתיחת הקובץ: הקובץ או הספרייה אינם קיימים
```

## צלילה לעומק

בימי ה-C המוקדמים, טיפול בשגיאות היה פשוט מאוד - בעיקר קודי חזרה ובדיקות ידניות. לאחר מכן הגיע `errno`, משתנה גלובלי שמתעדכן כאשר פונקציות נכשלות. הוא לא thread-safe בפני עצמו, לכן הוצגו הפונקציות `strerror` ו-`perror` לדיווח טוב יותר על שגיאות.

אלטרנטיבות? C מודרנית לא מוגבלת ל-`errno` בלבד. ישנם setjmp ו-longjmp לקפיצות לא מקומיות כשקטסטרופה מתקרבת. יש כאלה שמעדיפים להגדיר קודי שגיאה משלהם, בעוד אחרים בוחרים במבנים דמויי-חריגות ב-C++.

פרטי היישום יכולים להיות מורכבים. למשל, `errno` הוא thread-safe במערכות התואמות ל-POSIX בזכות קסמו של Thread Local Storage (TLS). במערכות מוטמעות, שבהן המשאבים יקרים, עשוי להעדף קוד טיפול בשגיאות מותאם אישית על פני גישות סטנדרטיות שעלולות להגדיל את התוכנה.

## ראה גם

- צלילה מפורטת ל-`errno`: https://en.cppreference.com/w/c/error/errno
- לבטיחות תהליכים, ראה POSIX threads ו-errno: http://man7.org/linux/man-pages/man3/pthread_self.3.html
- היכרות עם setjmp ו-longjmp: https://www.cplusplus.com/reference/csetjmp/
- עבור טיפול בחריגות ב-C++, בדוק: https://isocpp.org/wiki/faq/exceptions