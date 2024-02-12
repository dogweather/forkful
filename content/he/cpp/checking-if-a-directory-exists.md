---
title:                "בדיקה אם ספרייה קיימת"
aliases:
- he/cpp/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:16.215596-07:00
model:                 gpt-4-0125-preview
simple_title:         "בדיקה אם ספרייה קיימת"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
בדיקה אם ספרייה קיימת היא על קביעת נוכחות של ספרייה בנתיב מסוים לפני ביצוע פעולות כמו קריאה מאו כתיבה לתוך קבצים שבה. מתכנתים עושים זאת כדי להימנע משגיאות הקשורות לפעולות קבצים, מה שמבטיח ביצוע חלק ואמין יותר של משימות טיפול בקבצים ביישומים שלהם.

## איך לעשות:
ב-C++ מודרני (C++17 ואילך), אפשר להשתמש בספריית המערכת הקובצית לבדיקה אם ספרייה קיימת. היא מספקת דרך ישירה ומתוקנת לביצוע פעולות במערכת הקבצים, כולל בדיקה לקיומה של ספרייה.

```cpp
#include <iostream>
#include <filesystem>

namespace fs = std::filesystem;

int main() {
    const fs::path dirPath = "/path/to/directory";

    if (fs::exists(dirPath) && fs::is_directory(dirPath)) {
        std::cout << "The directory exists." << std::endl;
    } else {
        std::cout << "The directory does not exist." << std::endl;
    }

    return 0;
}
```
פלט לדוגמא אם הספרייה קיימת:
```
The directory exists.
```

פלט לדוגמא אם הספרייה לא קיימת:
```
The directory does not exist.
```

לפרויקטים שעדיין לא משתמשים ב-C++17 או לתכונות נוספות, ספריית המערכת הקובצית של Boost היא בחירה פופולרית של צד שלישי המציעה פונקציונליות דומה.

```cpp
#include <iostream>
#include <boost/filesystem.hpp>

namespace fs = boost::filesystem;

int main() {
    const fs::path dirPath = "/path/to/directory";

    if (fs::exists(dirPath) && fs::is_directory(dirPath)) {
        std::cout << "The directory exists." << std::endl;
    } else {
        std::cout << "The directory does not exist." << std::endl;
    }

    return 0;
}
```
בשימוש בספריית המערכת הקובצית של Boost, הפלט יהיה זהה לדוגמא ב-C++17 תלוי בקיומה של הספרייה בנתיב המצוין.
