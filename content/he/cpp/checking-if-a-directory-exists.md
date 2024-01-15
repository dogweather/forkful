---
title:                "לבדיקה אם תיקייה קיימת"
html_title:           "C++: לבדיקה אם תיקייה קיימת"
simple_title:         "לבדיקה אם תיקייה קיימת"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## למה

בכתיבת קוד בשפת סי++ ייתכן לנתח את כתיבת קבצים במערכת הקבצים, ומה הדרך הכי יעילה לכך - זאת בדיקה אם התיקייה קיימת או לא. זה חיוני במיוחד כאשר אנחנו יוצרים אפליקציות שעובדות עם קבצים ותיקיות במערכת ההפעלה.

## איך לעשות זאת?

כדי לבדוק אם תיקייה קיימת בשפת סי++, נצטרך להשתמש בפונקציית stat שפירושה "סטטיסטיקות מערכת", שמחזירה מידע על הקובץ או התיקייה המבוקשת. אנחנו נשתמש גם בפונקציית opendir לפתיחת התיקייה ונבדוק אם החזרת הפונקציה היא NULL - זאת אומרת שהתיקייה לא נמצאת.

כאן נמצא הקוד המלא עם דוגמאות קוד ופלט:

```C++
#include <iostream>
#include <sys/stat.h>
#include <dirent.h>

using namespace std;

int main()
{
    string directoryPath = "myFolder"; //נסה להחליף לשם של תיקייה שקיימת אצלך

    // בדיקת קיום התיקייה
    struct stat buffer;
    bool directoryExists = (stat(directoryPath.c_str(), &buffer) == 0);

    // פתיחת התיקייה ובדיקה של החזרת הפונקציה היא NULL
    DIR *dir = opendir(directoryPath.c_str());
    bool directoryValid = (dir != NULL);

    // הדפסה של התיקייה הקיימת אם נמצאה
    if (directoryExists && directoryValid)
        cout << "התיקייה מסלול " << directoryPath << " קיימת" << endl;
    else
        cout << "התיקייה לא נמצאה" << endl;

    return 0;
}

/* פלט אם התיקייה נמצאת
 התיקייה מסלול myFolder קיימת
 */

/* פלט אם התיקייה לא נמצאת
 התיקייה לא נמצאה
 */
```

## הבולט שאלות

כדי לוודא שהתיקייה קיימת במערכת הקבצים, אנחנו נוספים לאלגוריתם שלנו גם בדיקות אחרות, כמו בדיקת הרשאות ג