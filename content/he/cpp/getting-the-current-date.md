---
title:                "קבלת התאריך הנוכחי"
date:                  2024-01-20T15:13:42.398222-07:00
html_title:           "C: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?
לקבל את התאריך הנוכחי בתכנות זה לשאול את המערכת, "איזה יום אנחנו היום?" זה נחוץ ליומן אירועים, להדפיס זמנים על קבצים, ליצור תגובות תקופתיות, ועוד.

## איך לעשות:
ב-C++20, יש מודול חדש ליצירת תאריכים בקלות. ננסה אותו:

```C++
#include <chrono>
#include <iostream>
#include <format>

int main() {
    auto current_time = std::chrono::system_clock::now(); // שואלים את השעה מהמחשב
    std::time_t current_time_t = std::chrono::system_clock::to_time_t(current_time);

    // מדפיסים עם פורמט ידידותי
    std::cout << std::format("התאריך והשעה כרגע: {:%F %T}\n", std::chrono::system_clock::from_time_t(current_time_t));
    return 0;
}
```

פלט שיכול לצאת מזה יראה כך:
```
התאריך והשעה כרגע: 2023-04-05 14:55:31
```

## טבילת אש:
כבר ב-1970, בתקנים הראשונים של C, היה סטנדרט להגדרת זמנים נקרא 'epoch', והוא התחיל ב-1 בינואר 1970. המידע על הזמן מאז נשמר במשתנה מסוג `time_t`. היום, ל-C++ יש תשתיות מזמן (`<chrono>`) עם אובייקטים ופונקציות עדכניות שמקלות על השימוש והפורמטים. לפני C++20, היינו נתקלים בשימוש בפונקציות כמו `strftime` ו-`localtime` לאותו מטרה, ואפשר עדיין למצוא קוד כזה. למרות היופי שב-STL חדשה ומתוקנת, כדאי לזכור שקוד קיים עדיין משתמש לעיתים רבות בגישות הישנות.

## ראה גם:
- [cppreference.com - std::chrono](https://en.cppreference.com/w/cpp/chrono)
- [cppreference.com - std::format](https://en.cppreference.com/w/cpp/utility/format)
- [cplusplus.com - C Date & Time](http://www.cplusplus.com/reference/ctime/) (על גישות ישנות יותר)
- [ISO C++ - Working Draft](https://isocpp.org/std/the-standard) (הטיוטה הנוכחית של התקן C++)
