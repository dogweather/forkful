---
date: 2024-01-20 17:32:59.244730-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1\u05E2\u05D1\
  \u05E8, \u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\
  \u05DD \u05E0\u05E2\u05E9\u05EA\u05D4 \u05D1\u05E2\u05D6\u05E8\u05EA \u05E4\u05D5\
  \u05E0\u05E7\u05E6\u05D9\u05D5\u05EA \u05E9\u05DC \u05DE\u05D1\u05E0\u05D4 `tm`\
  \ \u05D0\u05D5 \u05D1\u05E7\u05D5\u05D3 \u05D9\u05D3\u05E0\u05D9 \u05E9\u05D9\u05E6\
  \u05E8 \u05D4\u05E9\u05D5\u05D5\u05D0\u05D5\u05EA \u05E2\u05DC \u05D1\u05E1\u05D9\
  \u05E1 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D5\u05DE\u05E1\u05E4\u05E8\
  \u05D9\u05DD. \u05E1\u05E4\u05E8\u05D9\u05D9\u05EA `<chrono>` \u05E9\u05D4\u05D5\
  \u05E6\u05D2\u05D4 \u05D1-C++11\u2026"
lastmod: '2024-04-05T22:50:53.941547-06:00'
model: gpt-4-1106-preview
summary: "\u05D1\u05E2\u05D1\u05E8, \u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05EA\u05D0\
  \u05E8\u05D9\u05DB\u05D9\u05DD \u05E0\u05E2\u05E9\u05EA\u05D4 \u05D1\u05E2\u05D6\
  \u05E8\u05EA \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA \u05E9\u05DC \u05DE\
  \u05D1\u05E0\u05D4 `tm` \u05D0\u05D5 \u05D1\u05E7\u05D5\u05D3 \u05D9\u05D3\u05E0\
  \u05D9 \u05E9\u05D9\u05E6\u05E8 \u05D4\u05E9\u05D5\u05D5\u05D0\u05D5\u05EA \u05E2\
  \u05DC \u05D1\u05E1\u05D9\u05E1 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D5\
  \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD."
title: "\u05D4\u05E9\u05D5\u05D5\u05D0\u05EA \u05E9\u05EA\u05D9 \u05EA\u05D0\u05E8\
  \u05D9\u05DB\u05D9\u05DD"
weight: 27
---

## איך לעשות:
שימוש בספריית `<chrono>`:

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    // יצירת שני תאריכים לשוויון
    std::chrono::system_clock::time_point date1 = std::chrono::system_clock::now();
    std::chrono::system_clock::time_point date2 = date1 + std::chrono::hours(24); // תאריך שני, מחר

    // הדפסת התאריכים
    std::time_t date1_time = std::chrono::system_clock::to_time_t(date1);
    std::time_t date2_time = std::chrono::system_clock::to_time_t(date2);
    std::cout << "Date 1: " << std::ctime(&date1_time);
    std::cout << "Date 2: " << std::ctime(&date2_time);

    // השוואת תאריכים
    if (date1 < date2) {
        std::cout << "Date 1 comes before Date 2.\n";
    } else {
        std::cout << "Date 1 comes after or is the same as Date 2.\n";
    }

    return 0;
}
```

פלט לדוגמה:

```
Date 1: Wed Feb 23 21:46:08 2023
Date 2: Thu Feb 24 21:46:08 2023
Date 1 comes before Date 2.
```

## עיון מעמיק
בעבר, השוואת תאריכים נעשתה בעזרת פונקציות של מבנה `tm` או בקוד ידני שיצר השוואות על בסיס מחרוזות ומספרים. ספריית `<chrono>` שהוצגה ב-C++11 והתפתחה בהמשך הופכת את התהליך לגמיש ומדויק יותר. אפשרויות נוספות כוללות שימוש בספריות צד שלישי, כמו Boost.DateTime. לגבי שינויים וייעולים, `<chrono>` נמצא במתמטיקה רבה לשיפורים ותוספות, ולמעשה, בגרסת C++20 יש לנו את `std::chrono::calendar` שמוסיפה ממשקים נוחים גם ליצירה ולהשוואה של תאריכים בлוח השנה.

## ראו גם
- מדריך לספריית `<chrono>`: https://en.cppreference.com/w/cpp/chrono
- ספריית Boost.DateTime: https://www.boost.org/doc/libs/release/doc/html/date_time.html
- ייעולים ב-C++20 לקלנדרים וזמנים: https://en.cppreference.com/w/cpp/chrono/calendar
