---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:35.436758-07:00
description: "\u05D0\u05D9\u05DA \u05DC: C++ \u05DE\u05E1\u05E4\u05E7 \u05DE\u05E1\
  \u05E4\u05E8 \u05D3\u05E8\u05DB\u05D9\u05DD \u05DC\u05E7\u05D1\u05DC \u05D0\u05EA\
  \ \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\u05D5\u05DB\u05D7\u05D9, \u05DB\
  \u05D5\u05DC\u05DC \u05D4\u05E1\u05E4\u05E8\u05D9\u05D9\u05D4 \u05D4\u05E1\u05D8\
  \u05E0\u05D3\u05E8\u05D8\u05D9\u05EA \u05E9\u05DC C++ \u05D5\u05E1\u05E4\u05E8\u05D9\
  \u05D5\u05EA \u05E9\u05DC \u05E6\u05D3 \u05E9\u05DC\u05D9\u05E9\u05D9 \u05DB\u05D2\
  \u05D5\u05DF Boost. \u05D4\u05D3\u05D5\u05D2\u05DE\u05D0\u05D5\u05EA \u05D4\u05D1\
  \u05D0\u05D5\u05EA \u05DE\u05D3\u05D2\u05D9\u05DE\u05D5\u05EA \u05D0\u05D9\u05DA\
  \ \u05DC\u05D1\u05E6\u05E2 \u05D0\u05EA \u05D4\u05DE\u05E9\u05D9\u05DE\u05D4\u2026"
lastmod: '2024-03-13T22:44:39.853550-06:00'
model: gpt-4-0125-preview
summary: "C++ \u05DE\u05E1\u05E4\u05E7 \u05DE\u05E1\u05E4\u05E8 \u05D3\u05E8\u05DB\
  \u05D9\u05DD \u05DC\u05E7\u05D1\u05DC \u05D0\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\
  \u05DA \u05D4\u05E0\u05D5\u05DB\u05D7\u05D9, \u05DB\u05D5\u05DC\u05DC \u05D4\u05E1\
  \u05E4\u05E8\u05D9\u05D9\u05D4 \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\u05EA\
  \ \u05E9\u05DC C++ \u05D5\u05E1\u05E4\u05E8\u05D9\u05D5\u05EA \u05E9\u05DC \u05E6\
  \u05D3 \u05E9\u05DC\u05D9\u05E9\u05D9 \u05DB\u05D2\u05D5\u05DF Boost."
title: "\u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\
  \u05D5\u05DB\u05D7\u05D9"
weight: 29
---

## איך ל:
C++ מספק מספר דרכים לקבל את התאריך הנוכחי, כולל הספרייה הסטנדרטית של C++ וספריות של צד שלישי כגון Boost. הדוגמאות הבאות מדגימות איך לבצע את המשימה הזו.

### באמצעות `<chrono>` (C++20 ואילך)
C++20 הציג פונקציונליות נוספת בספרייה `<chrono>`, ההופכת את קבלת התאריך הנוכחי לפשוטה יותר:
```cpp
#include <iostream>
#include <chrono>
#include <format> // עבור std::format (C++20)

int main() {
    auto current_time_point = std::chrono::system_clock::now(); // לכידת הזמן הנוכחי
    auto current_time_t = std::chrono::system_clock::to_time_t(current_time_point); // המרה ל-time_t

    // פורמט הזמן לפורמט קריא
    std::cout << "Current Date: " << std::format("{:%Y-%m-%d}", std::chrono::system_clock::to_time_t(current_time_point)) << std::endl;

    return 0;
}
```
**פלט לדוגמה:**
```plaintext
Current Date: 2023-03-15
```

### באמצעות `<ctime>`
למתכנתים העובדים עם גרסאות ישנות יותר של C++ או אלה המעדיפים את הספרייה המסורתית של C:
```cpp
#include <iostream>
#include <ctime>

int main() {
    std::time_t t = std::time(0); // קבלת הזמן הנוכחי
    std::tm* now = std::localtime(&t);
    std::cout << "Current Date: " 
              << (now->tm_year + 1900) << '-' 
              << (now->tm_mon + 1) << '-'
              <<  now->tm_mday
              << std::endl;

    return 0;
}
```
**פלט לדוגמה:**
```plaintext
Current Date: 2023-03-15
```

### באמצעות Boost Date_Time
לפרויקטים המשתמשים בספריות של Boost, ספריית Boost Date_Time מציעה דרך חלופית לקבל את התאריך הנוכחי:
```cpp
#include <iostream>
#include <boost/date_time.hpp>

int main() {
    // קבלת היום הנוכחי באמצעות לוח השנה הגרגוריאני של Boost
    boost::gregorian::date today = boost::gregorian::day_clock::local_day();
    std::cout << "Current Date: " << today << std::endl;

    return 0;
}
```
**פלט לדוגמה:**
```plaintext
Current Date: 2023-Mar-15
```
הדוגמאות הללו מספקות בסיס יסודי לעבודה עם תאריכים ב-C++, הכרחי עבור מגוון רחב של יישומים.
