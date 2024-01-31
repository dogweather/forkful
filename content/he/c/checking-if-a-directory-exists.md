---
title:                "בדיקה האם ספרייה קיימת"
date:                  2024-01-19
html_title:           "Arduino: בדיקה האם ספרייה קיימת"
simple_title:         "בדיקה האם ספרייה קיימת"

category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?

בודקים האם ספרייה קיימת כדי לוודא שאפשר לעבוד עם נתונים בלי שטעויות יפריעו. זה חיוני כדי למנוע בעיות בזמן ריצה ולעזור לתוכנית להיות יציבה יותר.

## איך לעשות:

```C
#include <stdio.h>
#include <sys/stat.h>

int main() {
    struct stat info;

    if (stat("/path_to_directory", &info) == 0 && S_ISDIR(info.st_mode)) {
        printf("Directory exists\n");
    } else {
        printf("Directory does not exist\n");
    }

    return 0;
}
```
פלט לדוגמה:
```
Directory exists
```
או
```
Directory does not exist
```

## עיון נוסף

הבדיקה אם ספרייה קיימת היא הליך בסיסי במערכות הפעלה מבוססות Unix/Linux. היא משמשת מתכנתים כדי לוודא שאפליקציה תוכל לאחסן או לגשת לנתונים בספרייה. ב-C, הפונקציה `stat` משמשת לכך, וממלאת מבנה נתונים עם מידע על הקובץ או הספרייה. אם הקובץ לא קיים, `stat` תחזיר שגיאה והשדה `st_mode` במבנה לא יילקח בחשבון.

אלטרנטיבה נפוצה ב-C היא פונקציית `opendir()` שמשמשת בספריות Standard POSIX. יש גם ביבליות חיצוניות כמו `boost` ב++C שנותנות גישה עשירה יותר וניידת בין מערכות הפעלה.

## ראה גם

- מדריך POSIX לפונקציית `stat`: [https://pubs.opengroup.org/onlinepubs/9699919799/functions/stat.html](https://pubs.opengroup.org/onlinepubs/9699919799/functions/stat.html)
- מדריך POSIX לפונקציית `opendir()`: [https://pubs.opengroup.org/onlinepubs/9699919799/functions/opendir.html](https://pubs.opengroup.org/onlinepubs/9699919799/functions/opendir.html)
- תיעוד ביבליות `boost`: [https://www.boost.org/doc/libs/](https://www.boost.org/doc/libs/)
