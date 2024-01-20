---
title:                "יצירת קובץ זמני"
html_title:           "C#: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
יצירת קובץ זמני היא תהליך שבו מריצים קוד שיוצר קובץ נוסף המכיל מידע זמני או מרצה תהליך שנמשך בזמן ריצת התכנית. מתכנתים לעתים משתמשים בזה לשמור מידע שיוזר בתהליך מדויק ונדרש לתהליכים בעתיד.

## איך ליצור:
נהוג לנצל את הספרייה `stdlib.h` ליצירת קובץ זמני. הקוד למטה מוצג דוגמה להגדרת קובץ זמני:

```C++
#include <stdlib.h>

void create_temp_file() {
    char temp_filename[L_tmpnam] = "";
    tmpnam(temp_filename); 
    FILE *temp = fopen(temp_filename, "w");
    fputs("some content", temp);
    fclose(temp);
}
```

בדוגמה זו, המפתח `tmpnam` מייצר שם קובץ על פי ההגדרות של המחשב, ו־`fopen` משתמש בשם הקובץ הזה ליצירת קובץ חדש עם ההגדרות שנתת.

## צלילה עמוקה:
דרך זו של יצירת קובצים זמניים מוכרת גם בשפות תכנות אחרות והופקה מהגרסה המקורית של שפת Unix C. למרבה הצער, היא מעט פגיעה במסוף עצמה. הרבה סיבוכים יכולים להופיע, למשל בדיקות הרשאות, מערכות הפעלה שונות, ועומסים מרובים.

אלטרנטיבה נהדרת לפונקציה `tmpnam` היא הפונקציה `mkstemp`, שגם נותנת שם וולידי ייחודי לקובץ זמני וגם מפתחת את הקובץ מיד לכתיבה. זוהי אופציה מצוינת, אם אתה עובד במערכת Unix או Linux.

## ראו גם:
- [הדרכת C++ ב-Chip Pearson](http://www.cplusplus.com/reference/cstdio/tmpnam/)
- [דיון במנות C++ ב-Stack Overflow](https://stackoverflow.com/questions/6303147/what-are-the-differences-between-tmpnam-and-mkstemp)