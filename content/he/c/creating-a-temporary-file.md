---
title:                "יצירת קובץ זמני"
date:                  2024-01-20T17:39:48.836022-07:00
model:                 gpt-4-1106-preview
simple_title:         "יצירת קובץ זמני"

category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה ולמה?
יצירת קובץ זמני היא פעולה שבה מחשב יוצר קובץ שמטרתו לשרת את התוכנית באופן זמני ולאחר מכן נמחק. תכנותים עושים זאת לצורך אחסון נתונים זמני, בדיקות, או מניעת עימותים בגישה לקבצים.

## איך לעשות:
הנה דוגמה פשוטה שמראה איך ליצור קובץ זמני:
```c
#include <stdio.h>
#include <stdlib.h>

int main() {
    char tmpname[L_tmpnam];
    FILE *tmpfile;

    // יצירת שם קובץ זמני
    tmpnam(tmpname);

    // פתיחת הקובץ לכתיבה
    tmpfile = fopen(tmpname, "w+");

    if (tmpfile) {
        // שימוש בקובץ
        fprintf(tmpfile, "Hello, World!\n");

        // סגירת הקובץ
        fclose(tmpfile);

        // מחיקת הקובץ
        remove(tmpname);
    } else {
        perror("File opening failed");
        return EXIT_FAILURE;
    }

    return 0;
}
```

## טבילה עמוקה:
במערכות UNIX מקודם, קבצים זמניים היו חיוניים למניעת עומסי זיכרון ולשמירה על זרימת עבודה חלקה. הכלים `tmpfile()` ו-`tmpnam()` בספרייה הסטנדרטית של C מאפשרים יצירת קבצים אלה, אך יש להשתמש בהם בזהירות מאחר והם יכולים להיות חשופים לתקיפות. חלופה עדיפה היא `mkstemp()` שמציעה אבטחה גבוהה יותר, אך לא כלולה בכל התקנים. האתגר בעבודה עם קבצים זמניים הוא גם לנקות אחרי שהמלאכה נעשתה כדי לא למלא את מערכת הקבצים בקבצים לא רצויים.

## גם כדאי לראות:
- [מדריך לפונקציית tmpfile() ב-C](https://en.cppreference.com/w/c/io/tmpfile)
- [דוקומנטציה של POSIX mkstemp()](https://man7.org/linux/man-pages/man3/mkstemp.3.html)
- [עקרונות עבודה עם קבצים ב-C](http://www.cplusplus.com/reference/cstdio/)
