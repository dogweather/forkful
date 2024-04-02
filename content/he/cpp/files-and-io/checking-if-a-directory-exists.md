---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:16.215596-07:00
description: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\
  \u05D9\u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA \u05D4\u05D9\u05D0 \u05E2\u05DC \u05E7\
  \u05D1\u05D9\u05E2\u05EA \u05E0\u05D5\u05DB\u05D7\u05D5\u05EA \u05E9\u05DC \u05E1\
  \u05E4\u05E8\u05D9\u05D9\u05D4 \u05D1\u05E0\u05EA\u05D9\u05D1 \u05DE\u05E1\u05D5\
  \u05D9\u05DD \u05DC\u05E4\u05E0\u05D9 \u05D1\u05D9\u05E6\u05D5\u05E2 \u05E4\u05E2\
  \u05D5\u05DC\u05D5\u05EA \u05DB\u05DE\u05D5 \u05E7\u05E8\u05D9\u05D0\u05D4 \u05DE\
  \u05D0\u05D5 \u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05EA\u05D5\u05DA \u05E7\u05D1\
  \u05E6\u05D9\u05DD \u05E9\u05D1\u05D4. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\
  \ \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\
  \u05D9\u05DE\u05E0\u05E2 \u05DE\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA\u2026"
lastmod: '2024-03-13T22:44:39.860119-06:00'
model: gpt-4-0125-preview
summary: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\u05D9\
  \u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA \u05D4\u05D9\u05D0 \u05E2\u05DC \u05E7\u05D1\
  \u05D9\u05E2\u05EA \u05E0\u05D5\u05DB\u05D7\u05D5\u05EA \u05E9\u05DC \u05E1\u05E4\
  \u05E8\u05D9\u05D9\u05D4 \u05D1\u05E0\u05EA\u05D9\u05D1 \u05DE\u05E1\u05D5\u05D9\
  \u05DD \u05DC\u05E4\u05E0\u05D9 \u05D1\u05D9\u05E6\u05D5\u05E2 \u05E4\u05E2\u05D5\
  \u05DC\u05D5\u05EA \u05DB\u05DE\u05D5 \u05E7\u05E8\u05D9\u05D0\u05D4 \u05DE\u05D0\
  \u05D5 \u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05EA\u05D5\u05DA \u05E7\u05D1\u05E6\
  \u05D9\u05DD \u05E9\u05D1\u05D4. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\
  \u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\u05D9\
  \u05DE\u05E0\u05E2 \u05DE\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA\u2026"
title: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\u05D9\
  \u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA"
weight: 20
---

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
