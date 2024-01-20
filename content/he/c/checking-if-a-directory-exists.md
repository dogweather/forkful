---
title:                "בדיקה אם ספרייה קיימת"
html_title:           "C: בדיקה אם ספרייה קיימת"
simple_title:         "בדיקה אם ספרייה קיימת"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?

בדיקה אם ספריה קיימת היא פעולה שבה התוכנה נוסחת כך שתזהה אם תיקייה מוגדרת במיקום מסוים במערכת הקבצים. תכנתים מבצעים פעולה זו כדי למנוע שגיאות במהלך פעולות אמנמנטיות כמו קריאה או כתיבה לתיקייה.

## דרך פעולה:

ישנם שני דרכים בסיסיות לבדוק את הדבר הזה בשפת C, אחת באמצעות הפונקציה `stat`, ואחת באמצעות הפונקציה `opendir`.

שימוש ב- `stat`:
```C 
#include <sys/stat.h>
#include <stdio.h>

int main(){
    struct stat buffer;
    int exist = stat("/path/to/directory",&buffer);
    if(exist == 0)
        printf("Directory exists.\n");
    else 
        printf("Directory does not exist.\n");
    return 0;
}
```

שימוש ב- `opendir`:
```C
#include <dirent.h>
#include <stdio.h>

int main(){
    DIR* dir = opendir("/path/to/directory");
    if (dir)
    {
        printf("Directory exists.\n");
        closedir(dir);
    }
    else 
        printf("Directory does not exist.\n");
    return 0;
}
```

## צלילה עמוקה

השימוש ב- `stat` נפוץ בפרויקטים ישנים שנכתבו לפני כן, בעוד שהשימוש ב- `opendir` הוא יעיל יותר מכיוון שהוא מבצע את פתיחת הספריה באותה פעולה. אפשרויות נוספות כוללות שימוש ב- `access`, או בפונקציות יישומיות של מערכת ההפעלה המסויימת, אך אלו יכולות להיות פחות ניידות. הפרטים בעבודה מתחת לכותרת הם באופן כללי תלויים במערכת ההפעלה ובהעדפות המתכתב.

## ראה גם:

- [Man page of stat on Linux](https://man7.org/linux/man-pages/man2/stat.2.html)
- [Man page of opendir on Linux](https://man7.org/linux/man-pages/man3/opendir.3.html)
- [Microsoft documentation on _stat for Windows](https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/stat-functions?view=msvc-160)