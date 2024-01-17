---
title:                "בדיקת קיום תיקייה"
html_title:           "C++: בדיקת קיום תיקייה"
simple_title:         "בדיקת קיום תיקייה"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# מה ולמה? 

בתוך כתיבת קוד, לעיתים קורה שאנו צריכים לבדוק אם תיקייה קיימת במערכת הקבצים או לא. תהליך זה נקרא "בדיקה אם תיקייה קיימת" והוא חשוב מאוד בתכנות כדי לוודא שהתקיות שאנו משתמשים בהן קיימות וכדי למנוע שגיאות בזמן ריצת התוכנית.

# איך לכתוב קוד:

```C++
#include <iostream>
#include <filesystem>

int main()
{
    // כאן אנו משתמשים בהליך הבדיקה אם תיקייה קיימת עם עזרת הפונקציה exists שנמצאת בספריית הקוד filesystem
    if (std::filesystem::exists("תיקיית משתמש"))
    {
        std::cout << "תיקייה זו קיימת!" << std::endl;
    }
    else
    {
        std::cout << "תיקייה זו לא קיימת." << std::endl;
    }

    return 0;
}
```

פלט התוכנית יחזיר:

```
תיקייה זו קיימת!
```

## העמקת תוכן:

### היסטוריה:

בעבר, בדיקת קיום תיקייה הייתה תהליך מסובך יותר ודרשה המרה של ידע עם מערכת הקבצים או השימוש בפקודות של מערכת הפעלה. הפונקציה exists הייתה זמינה רק בספריית הקוד boost, אך באופן רשמי הוכללה ב-C++17 כחלק מספריית הקוד filesystem.

### אלטרנטיבות:

במקום להשתמש בפונקציה exists הנ"ל, ניתן להשתמש במשתנה פנימי בשם directory_entry המכיל את התיקייה ולבדוק את המשתנה אם הוא מתאים לתנאי קיום.

### פרטי היישום:

בכיתוב הקוד ניתן לראות שאנו משתמשים בשימוש בפונקציה exists שבתוך משתנה פנימי בשם filesystem שנמצא בספריית הקוד של C++17. דרך זו מאפשרת לנו לכתוב קוד ברור וקל לקריאה ומבצעת באופן אוטומטי את תהליך בדיקת התיקייה עבורנו.

## ראו גם:

למידע נוסף על הפונקציה exists וספריית הקוד filesystem ניתן להציץ במקורות הבאים:

- [CPP Reference](https://en.cppreference.com/w/cpp/filesystem/exists)
- [C++17 - The Complete Guide](https://en.cppreference.com/w/cpp/filesystem/exists)
- [Boost.Filesystem](https://www.boost.org/doc/libs/1_75_0/libs/filesystem/doc/index.htm)