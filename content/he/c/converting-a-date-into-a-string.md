---
title:                "המרת תאריך למחרוזת"
html_title:           "C: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

##Why
תמיד קשה לטפל בתאריך בתוכנית. כתיבה של תאריך בתור מחרוזת מאפשרת לנו לטפל בתאריך בצורה פשוטה וקלה יותר.

##How To
```C
#include <stdio.h>
#include <time.h>

char* date_to_string(time_t date) {
    // יצירת מערך מאפשר לשמירת התאריך בתור מחרוזת
    char str[26];

    // מתחילים את העיבוד של התאריך באמצעות פונקציית gmtime
    struct tm* timeinfo = gmtime(&date);

    // שימוש בפונקציית strftime עבור יצירת המחרוזת בתאריך ובפורמט מתאים
    strftime(str, 26, "%Y-%m-%d %H:%M:%S", timeinfo);

    // החזרת המחרוזת כתאריך מלא בפורמט מבוקש
    return str;
}

int main() {
    // תאריך נתון לדוגמה
    time_t my_date = 1568719200;

    // הדפסת התאריך בפורמט מחרוזת
    printf("התאריך בפורמט מחרוזת הוא: %s", date_to_string(my_date));
    return 0;
}
```
Output: התאריך בפורמט מחרוזת הוא: 2019-09-17 19:00:00

##Deep Dive
בכדי להמיר תאריך למחרוזת בשפת C, נצטרך להשתמש בפונקציית gmtime המאפשרת לנו לעבור על תאריך נתון לפי כל הפורמטים הנתמכים ולהחזיר את התאריך בתור struct tm. לאחר מכן, נשתמש בפונקציית strftime המאפשרת לנו להמיר את התאריך בתוך הstruct למחרוזת בפורמט מתאים עבורנו.

##See Also
- תיעוד רשמי לפונקציות gmtime ו- strftime בשפת C: https://www.cplusplus.com/reference/ctime/gmtime/
- מדריך לטיפול בתאריכים בשפת C: https://www.tutorialspoint.com/c_standard_library/time_h.htm