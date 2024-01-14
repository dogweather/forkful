---
title:    "C++: לבדיקה האם תיקייה קיימת"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# למה:
בדיקת קיום תיקייה בקוד קורא יכולה להיות חשובה כאשר מתכנתים רוצים לוודא שמיקום מסוים במערכת הקבצים קיים ויכול לכלול קבצים או תוכניות חשובות.

## איך לבדוק את קיום התיקייה
בשפת סי++ ישנם מספר דרכים שונות לבדוק אם תיקייה קיימת. להלן כמה דוגמאות קוד בסי++ עם פלט משתמש:

```C++
#include <iostream>
#include <filesystem>

namespace fs = std::filesystem;

int main()
{
    // מסלול מלא של התיקייה לבדיקה
    fs::path path_to_check = "C:/Users/User1/Desktop/Project";

    // בדיקה האם התיקייה קיימת
    if (fs::exists(path_to_check))
    {
        std::cout << "התיקייה קיימת" << std::endl;
    }
    else
    {
        std::cout << "התיקייה אינה קיימת" << std::endl;
    }

    // בדיקה האם התיקייה קיימת וגם היא תיקייה ריקה
    if (fs::is_empty(path_to_check))
    {
        std::cout << "התיקייה ריקה" << std::endl;
    }
    else
    {
        std::cout << "התיקייה אינה ריקה" << std::endl;
    }

    return 0;
}
```

פלט משתמש:

```
התיקייה קיימת
התיקייה אינה ריקה
```

## Deep Dive:
עבור בדיקה מדוייקת יותר, ניתן להשתמש בפונקציות נוספות כמו `canonical()` לקבלת כתובת מסלול מדוייקת לתיקייה ו- `status()` לקבלת מידע נוסף על התיקייה כמו גודל וזמן יצירה. כדי לספק תמיכה במערכות הפעלה שונות, כדאי להשתמש במקרון `filesystem::path::preferred_separator` כדי לקבל את התו המתאים למערכת ההפעלה המקומית.

# ראו גם:
- [תיעוד שפת סי++ בנוגע לבדיקת קיום תיקייה](https://en.cppreference.com/w/cpp/filesystem)
- [מידע על תוכנית בדיקת קיום תיקייה בשפת סי++](https://www.geeksforgeeks.org/c-program-check-given-file-exists-not/)
- [פרסום פוסט