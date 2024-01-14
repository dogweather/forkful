---
title:    "C++: בדיקה אם תיקייה קיימת"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Why - למה

בכתיבת קוד בשפת C++, ייתכן כי תקבלו מצבים שבהם תרצו לבדוק אם תיקייה קיימת או לא. כזה יכול להיות למשל כאשר ברצונכם לכתוב קובץ או מחרוזת למיקום מסוים בתיקיית המערכת, ותרצו לוודא שהתיקייה הרלוונטית קיימת.

## How To - איך לבדוק אם תיקייה קיימת

בשפת C++, ניתן לבדוק את קיומה של תיקייה באמצעות הפונקציה `std::filesystem::exists()`. למשל, אם נרצה לבדוק את קיומה של התיקייה "myFolder" בתיקיית המערכת הנוכחית, ניתן לכתוב את הקוד הבא:

```C++
#include <iostream>
#include <filesystem>

int main()
{
    std::string folderName = "myFolder";
    if(std::filesystem::exists(folderName))
    {
        std::cout << folderName << " exists\n";
    }
    else
    {
        std::cout << folderName << " doesn't exist\n";
    }

    return 0;
}
```

פלט התוכנית יהיה:

```
myFolder exists
```

במידה והתיקייה לא תקיימת, הפלט יראה כך:

```
myFolder doesn't exist
```

## Deep Dive - חקירה מעמיקה

ניתן לבצע גם בדיקת קיום של תיקייה באמצעות הפונקציה `std::experimental::filesystem::exists()`, שנועדה לבדיקת תיקיות בגרסאות ישנות יותר של שפת C++. אם נשתמש בפונקציה זו, יש להתחבר לאפשרות `-lstdc++fs` כדי לכלול את ספריית ה-filesystem הנדרשת.

בנוסף, חשוב לציין כי הפונקציה `std::filesystem::exists()` נותנת חזרה תשובה ישירה בפורמט boolean (true/false), בעוד שהפונקציה `std::experimental::filesystem::exists()` מחזירה משתנה מסוג std::error_code, שיכול למשמש למטרת ניטור וטיפול בשגיאות במקביל.

## See Also - ראו גם

- [std::filesystem::exists() documentation](https://en.cppreference.com/w/cpp/filesystem/exists)
- [std::experimental::filesystem::exists() documentation](https://www.cplusplus.com/reference/experimental/filesystem/exists/)