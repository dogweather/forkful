---
title:                "בדיקה האם ספרייה קיימת"
date:                  2024-01-19
html_title:           "Arduino: בדיקה האם ספרייה קיימת"
simple_title:         "בדיקה האם ספרייה קיימת"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?
בדיקה אם ספרייה קיימת ב-C++ מאפשרת לקוד לזהות אם ספרייה נתונה נמצאת במערכת הקבצים. תכניתנים עושים זאת כדי להימנע משגיאות בעת יצירת ספרייה שכבר קיימת, או לטפל בקבצים בתוך ספריות שקיימות.

## איך לעשות:
```C++
#include <iostream>
#include <filesystem>

namespace fs = std::filesystem;

int main() {
    std::string path_to_check = "/path/to/directory";

    if(fs::exists(path_to_check)) {
        std::cout << "הספרייה קיימת!" << std::endl;
    } else {
        std::cout << "הספרייה אינה קיימת." << std::endl;
    }

    return 0;
}
```
פלט לדוגמא:
```
הספרייה קיימת!
או
הספרייה אינה קיימת.
```

## עיון מעמיק
בידיקת קיום ספרייה הוא פעולה נפוצה בתכנות. לפני הסטנדרט C++17, תכניתנים נאלצו להשתמש ב-API של מערכת ההפעלה או ספריות צד שלישי כמו Boost.Filesystem. הסטנדרט C++17 הציג את ספריית `<filesystem>`, שמפשטת את ביצוע משימות נפוצות כאלו. אלטרנטיבות ל `<filesystem>` עדיין קיימות ויכולות לכלול שימוש ב-funcitons כמו `stat()` ב-Unix או `GetFileAttributes()` ב-Windows.

## ראו גם:
- [std::filesystem documentation](https://en.cppreference.com/w/cpp/filesystem)
- [Boost.Filesystem Library](https://www.boost.org/doc/libs/1_75_0/libs/filesystem/doc/index.htm)
