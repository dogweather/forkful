---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:12.075530-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-C++ \u05DE\
  \u05D5\u05D3\u05E8\u05E0\u05D9, \u05E0\u05D9\u05EA\u05DF \u05DC\u05D4\u05E9\u05EA\
  \u05DE\u05E9 \u05D1\u05E1\u05E4\u05E8\u05D9\u05D9\u05EA \u05D4-`<chrono>` \u05DB\
  \u05D3\u05D9 \u05DC\u05D8\u05E4\u05DC \u05D1\u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\
  \u05DD \u05D5\u05D1\u05D6\u05DE\u05E0\u05D9\u05DD \u05D1\u05D0\u05D5\u05E4\u05DF\
  \ \u05D8\u05D1\u05E2\u05D9, \u05D0\u05DA \u05D4\u05D9\u05D0 \u05DC\u05D0 \u05EA\u05D5\
  \u05DE\u05DB\u05EA \u05D9\u05E9\u05D9\u05E8\u05D5\u05EA \u05D1\u05E4\u05D9\u05E2\
  \u05E0\u05D5\u05D7 \u05DE\u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05DC\u05DC\
  \u05D0 \u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 \u05D9\u05D3\u05E0\u05D9\u2026"
lastmod: '2024-03-13T22:44:39.851639-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-C++ \u05DE\u05D5\u05D3\u05E8\u05E0\u05D9, \u05E0\u05D9\u05EA\u05DF\
  \ \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9 \u05D1\u05E1\u05E4\u05E8\u05D9\u05D9\u05EA\
  \ \u05D4-`<chrono>` \u05DB\u05D3\u05D9 \u05DC\u05D8\u05E4\u05DC \u05D1\u05EA\u05D0\
  \u05E8\u05D9\u05DB\u05D9\u05DD \u05D5\u05D1\u05D6\u05DE\u05E0\u05D9\u05DD \u05D1\
  \u05D0\u05D5\u05E4\u05DF \u05D8\u05D1\u05E2\u05D9, \u05D0\u05DA \u05D4\u05D9\u05D0\
  \ \u05DC\u05D0 \u05EA\u05D5\u05DE\u05DB\u05EA \u05D9\u05E9\u05D9\u05E8\u05D5\u05EA\
  \ \u05D1\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 \u05DE\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA \u05DC\u05DC\u05D0 \u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 \u05D9\u05D3\
  \u05E0\u05D9 \u05E2\u05D1\u05D5\u05E8 \u05E4\u05D5\u05E8\u05DE\u05D8\u05D9\u05DD\
  \ \u05DE\u05D5\u05E8\u05DB\u05D1\u05D9\u05DD \u05D9\u05D5\u05EA\u05E8."
title: "\u05E4\u05E8\u05E1\u05D5\u05DD \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 30
---

## איך לעשות:
ב-C++ מודרני, ניתן להשתמש בספריית ה-`<chrono>` כדי לטפל בתאריכים ובזמנים באופן טבעי, אך היא לא תומכת ישירות בפיענוח ממחרוזות ללא פיענוח ידני עבור פורמטים מורכבים יותר. עם זאת, עבור פורמטי תאריך בסגנון ISO 8601 ופורמטים אישיים פשוטים, הנה איך אפשר לבצע פיענוח.

**באמצעות `<chrono>` ו-`<sstream>`:**
```cpp
#include <iostream>
#include <sstream>
#include <chrono>
#include <iomanip>

int main() {
    std::string date_str = "2023-04-15"; // פורמט ISO 8601
    std::istringstream iss(date_str);
    
    std::chrono::year_month_day parsed_date;
    iss >> std::chrono::parse("%F", parsed_date);
    
    if (!iss.fail()) {
        std::cout << "Parsed date: " << parsed_date << std::endl;
    } else {
        std::cout << "Failed to parse date." << std::endl;
    }
    
    return 0;
}
```
פלט לדוגמא:
```
Parsed date: 2023-04-15
```

עבור פורמטים מורכבים יותר או כאשר מתמודדים עם גרסאות ישנות יותר של C++, ספריות צד שלישי כמו `date.h` (ספריית התאריך של הווארד הינאנט) פופולריות. הנה איך אפשר לפרש פורמטים שונים עם זה:

**השימוש בספריית `date.h`:**
וודאו שהספרייה מותקנת. ניתן למצוא אותה [כאן](https://github.com/HowardHinnant/date).

```cpp
#include "date/date.h"
#include <iostream>

int main() {
    std::string date_str = "April 15, 2023";
    
    std::istringstream iss(date_str);
    date::sys_days parsed_date;
    iss >> date::parse("%B %d, %Y", parsed_date);
    
    if (!iss.fail()) {
        std::cout << "Parsed date: " << parsed_date << std::endl;
    } else {
        std::cout << "Failed to parse date from string." << std::endl;
    }

    return 0;
}
```
פלט לדוגמא (עשוי להשתנות בהתאם לאזור ולהגדרות התאריך במערכת שלכם):
```
Parsed date: 2023-04-15
```
