---
title:                "C++: בדיקת קיום תיקייה"
simple_title:         "בדיקת קיום תיקייה"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

כתיבת פוסט נהתקדם בתכנות קווי עבור גולשים עבריים:

## מדוע

במאמר זה נדבר על כלי חשוב ושימושי של שפת תכנות C++, "לבדוק אם ספרייה כלשהי קיימת". תהליך זה חיוני בעבודה עם ספריות ותיקיות במימושי התכניות שלנו ומסייע לנו לנהל את הדרכים בהן נגשים לקבצים ותיקיות במערכת ההפעלה.

## כיצד לבדוק אם תיקייה קיימת

עבודה עם תיקיות וקבצים היא חלק בלתי נפרד מתהליך התכנות. לכן, יתרון גדול מאוד לדעת איך לבדוק אם תיקייה מסוימת קיימת במערכת הפעלה שלנו. נציג מספר דוגמאות פשוטות שבררה שיצוריק לכם איך לבדוק זאת בקוד C++.

```C++
#include <iostream>
#include <string>
#include <filesystem>

namespace fs = std::filesystem;

int main()
{
    std::string dir_name = "תיקיית הדוגמה";

    if (fs::exists(dir_name)) // בודקת האם התיקייה קיימת במערכת
    {
        std::cout << "התיקייה " << dir_name << " קיימת" << std::endl;
    }
    else
    {
        std::cout << "התיקייה " << dir_name << " לא קיימת" << std::endl;
    }

    return 0;
}
```

פלט התוכנית:

```
התיקייה תיקיית הדוגמה קיימת
```

ניתן גם לתת למשתמש להזין את השם של התיקייה במהלך ביצוע התכנית:

```C++
#include <iostream>
#include <string>
#include <filesystem>

namespace fs = std::filesystem;

int main()
{
    std::string dir_name;

    std::cout << "הזן את שם התיקייה: ";
    std::cin >> dir_name;

    if (fs::exists(dir_name))
    {
        std::cout << "התיקייה " << dir_name << " קיימת" << std::endl;
    }
    else
    {
        std::cout << "התיקייה " << dir_name << " לא קיימת" << std::endl;
    }

    return 0;
}
```

פלט התוכנית:

```
הזן את שם התיקייה: מהנהלת ולידה
התיקייה מהנהלת ולידה לא קיימת
```

## מגע ב