---
title:    "C: לבדיקה האם תיקייה קיימת."
keywords: ["C"]
---

{{< edit_this_page >}}

##למה

בתכנות בשפת סי, חשיבה על מנת לוודא אם ספרייה קיימת היא חשובה כי זה יכול לסייע לנו להריץ פעולות מסוימות אם היא קיימת, או למנוע קריסות כאשר אנחנו מנסים לגשת לספרייה שאינה קיימת.

## איך לעשות זאת

בשפת סי ישנם שני דרכים לוודא האם ספרייה קיימת: באמצעות הפונקציה "opendir" ובאמצעות הפונקציה "stat". להלן דוגמאות לשני הדרכים כדי לוודא אם ספרייה קיימת במערכת הקבצים שלנו:

```C
// באמצעות הפונקציה opendir
#include <stdio.h>
#include <dirent.h>

int main()
{
    FILE *folder;
    char *dir_name = "my_folder";

    // פותח ספרייה
    folder = opendir(dir_name);
    // בודק האם הפעולה נכשלה
    if (folder == NULL)
    {
        // אם כן, הצג הודעה
        printf("הספרייה לא קיימת");
    }
    else
    {
        // אם לא, הצג הודעה נוספת
         printf("הספרייה קיימת");
         // סגור ספרייה
         closedir(folder);
    }
    return 0;
}

// באמצעות הפונקציה stat
#include <stdio.h>
#include <sys/stat.h>

int main()
{
    struct stat st = {0};
    char *dir_name = "my_folder";

    // בודק את מצב הספרייה
    if (stat(dir_name, &st) == -1)
    {
        // אם היא לא קיימת, הצג הודעה
        printf("הספרייה לא קיימת");
    }
    else
    {
        // אחרת, הצג הודעה נוספת
         printf("הספרייה קיימת");
    }
    return 0;
}
```

## עיון מעמיק

הפונקציה "opendir" מחזירה מצב אישי לספרייה הרצויה, ואילו הפונקציה "stat" מכילה מצב רחב יותר, כולל מידע נוסף על הספרייה. בחירת הדרך הנכונה תלויה בצורך הספציפי של התוכנית שלנו.

##ראו גם

1. [פונקציה opendir ה