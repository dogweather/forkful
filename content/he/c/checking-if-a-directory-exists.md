---
title:                "C: האם תיקייה קיימת - מאמר על תכנות מחשבים"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## למה

במאמר זה אני אסביר איך ולמה כדאי לבדוק אם תיקייה קיימת בשפת תכנות C. לעיתים קרובות, יש לנו צורך לבצע פעולות מסוימות על תיקיות על מנת לנהל את המידע שלנו ולשלוט בתהליך התכנות שלנו. כדאי לבדוק אם תיקייה קיימת לפני שממשיכים לבצע פעולות עליה כדי לוודא שאנו עובדים על מבנה מסודר ושהקוד שלנו יפעל כראוי.

## איך לבדוק אם תיקייה קיימת

כאשר נרצה לבדוק אם תיקייה קיימת בשפת תכנות C, נצטרך להשתמש בפונקציה המתאימה - `opendir()`. זאת הפונקציה המשמשת לפתיחת תיקייה ומחזירה מצב של תיקייה מאוחר. אם התיקייה לא קיימת, הפונקציה תחזיר NULL ואנו נצטרך לטפל בזה בקוד שלנו. להלן דוגמא של קוד שבו אנו בודקים אם תיקייה קיימת:

```C
#include <stdio.h>
#include <dirent.h>
 
int main(void) {
    DIR *dir = opendir("my_directory");
 
    if (dir) {
        printf("תיקייה זו קיימת.\n");
        closedir(dir);
    } else {
        printf("תיקייה זו לא קיימת.\n");
    }
 
    return 0;
 }
```

הפלט של מכונת המחשב יראה כך:

```
תיקייה זו לא קיימת.
```

ואם נרצה לעשות את הבדיקה בפונקציה נפרדת ולקבל תשובה זהה, נשנה את הקוד כך:

```C
#include <stdio.h>
#include <dirent.h>

int directory_exists(const char *path) {
    DIR *dir = opendir(path);

    if (dir) {
        closedir(dir);
        return 1;
    }

    return 0;
}

int main(void) {
    if (directory_exists("my_directory")) {
        printf("תיקייה זו קיימת.\n");
    } else {
        printf("תיקייה זו לא קיימת.\n");
    }

    return 0;
}
```

כאן אנו משתמשים בפונקציה נפרדת `directory_exists()` לבדיקת הקיום של התיקייה.

## מעמ