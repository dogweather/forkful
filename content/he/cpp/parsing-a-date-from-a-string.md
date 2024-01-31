---
title:                "ניתוח תאריך ממחרוזת"
date:                  2024-01-20T15:35:03.263490-07:00
html_title:           "Arduino: ניתוח תאריך ממחרוזת"
simple_title:         "ניתוח תאריך ממחרוזת"

category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
פרסינג (Parsing) של תאריך ממחרוזת זה תהליך של קריאה והמרה של תאריך כתוכן טקסט לאובייקט תאריך בקוד. תכנתים עושים זאת כדי לאפשר עיבוד, אימות, או שמירה של תאריכים במערכות ממוחשבות.

## How to: (איך לעשות:)
```cpp
#include <iostream>
#include <sstream>
#include <iomanip>
#include <chrono>
#include <string>

int main() {
    // דוגמה למחרוזת תאריך
    std::string date_str = "2023-03-14 16:05:23";
    
    // יצירת time_point מתוך המחרוזת
    std::tm tm = {};
    std::istringstream ss(date_str);
    
    ss >> std::get_time(&tm, "%Y-%m-%d %H:%M:%S");  // פורמט התאריך
    if (ss.fail()) {
        std::cerr << "Parse failed\n";
        return 1;
    }
    
    // יצירת time_point והדפסה
    std::chrono::system_clock::time_point tp = std::chrono::system_clock::from_time_t(std::mktime(&tm));
    std::time_t time_out = std::chrono::system_clock::to_time_t(tp);
    std::cout << "Parsed date and time: " << std::ctime(&time_out);
    
    return 0;
}
```
פלט דוגמה:
```
Parsed date and time: Tue Mar 14 16:05:23 2023
```

## Deep Dive (עומק הנושא)
בעבר, C++ השתמשה בספריית C עבור עבודה עם זמנים ותאריכים. זה הביא לבלבול וטעויות. מאז כוללת C++11 את ספריית chrono, עדינה יותר ומאופיינת ביתר בטיחות. קיימים גם ספריות של צד שלישי, כמו Boost.Date_Time, אך chrono כבר מספקת בסיס טוב לרוב המקרים. Parsing מתבצע באמצעות std::get_time, שזה הכלי המוכלל המאפשר המרה מחרוזת לטיפוסי זמן באופן בטוח יותר.

## See Also (ראו גם)
- [cppreference std::get_time](https://en.cppreference.com/w/cpp/io/manip/get_time) - מידע נוסף על std::get_time.
- [cppreference std::chrono](https://en.cppreference.com/w/cpp/chrono) - מידע נוסף על ספריית chrono ב-C++.
- [ISO 8601 Date and time format](https://www.iso.org/iso-8601-date-and-time-format.html) - סטנדרט הפורמט של תאריך וזמן המומלץ לשימוש.
