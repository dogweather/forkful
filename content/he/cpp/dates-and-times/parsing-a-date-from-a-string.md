---
title:                "פרסום תאריך ממחרוזת"
aliases:
- /he/cpp/parsing-a-date-from-a-string/
date:                  2024-02-03T19:14:12.075530-07:00
model:                 gpt-4-0125-preview
simple_title:         "פרסום תאריך ממחרוזת"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
פיענוח תאריך ממחרוזת כולל את הפרשנות של פורמט המחרוזת לחלקי התאריך כמו יום, חודש ושנה. מתכנתים עושים זאת כדי לטפל בקלט מהמשתמש, לקרוא קבצי נתונים או להתקשר עם API-ים שמתקשרים תאריכים בפורמטי מחרוזות. זה חשוב לעיבוד נתונים, אימות, וביצוע חישובי תאריך ביישומים.

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
