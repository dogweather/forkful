---
title:    "C++: יצירת קובץ זמני"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## למה

ישנם מצבים רבים שבהם ייתכן שתרצה ליצור קובץ זמני בתוכנית פיתוח שלך. לדוגמה, אם אתה צריך לכתוב נתוני מחיצה זמנית לגורם חיצוני, או אם אתה עובד עם קבצי טקסט ואתה רוצה ליצור קובץ זמני כדי לבדוק את הקוד שלך.

## איך ליצור קובץ זמני ב-C++

הייצור קובץ זמני בשפת סי++ הוא פעולה פשוטה וקלה לביצוע. פשוט עקוב אחר השלבים הבאים:

```C++
// כותבים התאמות:
#include <iostream>
#include <fstream>
#include <cstdlib>

int main()
{
    // יצירת משתנה לכתיבת שם לקובץ זמני:
    char temp_name[L_tmpnam];
    tmpnam(temp_name);

    // פתיחת קובץ זמני לכתיבה:
    std::ofstream file(temp_name);
    if (!file.is_open())
    {
        // איפוס התוכנה במקרה שגיאה בפתיחת הקובץ:
        std::cout << "Could not create temporary file\n";
        exit(EXIT_FAILURE);
    }

    // כתיבת נתונים לקובץ:
    file << "This is a temporary file" << std::endl;
    file.close();

    // קריאת הקובץ והדפסת התוכן שלו לצורך בדיקה:
    std::ifstream temp_file(temp_name);
    std::cout << temp_file.rdbuf();
    temp_file.close();

    return 0;
}
```

פלט הקוד הנ"ל יהיה:

```
This is a temporary file
```

## צילום מציאות

כאשר אתה יוצר קובץ זמני, הוא מאוחסן על גבי הדיסק הקשיח הפיזי של המחשב שלך. קיימים שני טיפוסים שונים של קבצי זמניים:

1. **קבצי שם זמני (Named temporary files):** יש להם שם והם נמצאים במיקום קבוע במערכת הקבצים של המחשב. לרוב, בסביבות מערכי ההפעלה שלנו, זהו תיקיית TEMP או TMP באתר התקני C:\.

2. **קבצי מסלול זמני (Path temporary files):** אינם מקבלים שם מזהה ייחודי, אלא נגישים לפי הנתיב המוגדר לק