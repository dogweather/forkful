---
date: 2024-01-26 03:43:39.458849-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: C++ \u05DE\u05E6\u05D9\
  \u05E2\u05D4 \u05DE\u05E1\u05E4\u05E8 \u05D3\u05E8\u05DB\u05D9\u05DD \u05DC\u05E2\
  \u05D2\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD, \u05DB\u05DE\u05D5 `floor()`,\
  \ `ceil()`, \u05D5-`round()`."
lastmod: '2024-03-13T22:44:39.827139-06:00'
model: gpt-4-0125-preview
summary: "C++ \u05DE\u05E6\u05D9\u05E2\u05D4 \u05DE\u05E1\u05E4\u05E8 \u05D3\u05E8\
  \u05DB\u05D9\u05DD \u05DC\u05E2\u05D2\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD\
  , \u05DB\u05DE\u05D5 `floor()`, `ceil()`, \u05D5-`round()`."
title: "\u05E2\u05D9\u05D2\u05D5\u05DC \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD"
weight: 13
---

## איך לעשות:
C++ מציעה מספר דרכים לעגל מספרים, כמו `floor()`, `ceil()`, ו-`round()`:

```C++
#include <iostream>
#include <cmath> // לפונקציות עיגול

int main() {
    double num = 3.14;

    std::cout << "floor: " << std::floor(num) << "\n"; // פלט: floor: 3
    std::cout << "ceil: " << std::ceil(num) << "\n";   // פלט: ceil: 4
    std::cout << "round: " << std::round(num) << "\n"; // פלט: round: 3

    // לדיוק קבוע, כגון עיגול לשני עשרוניים:
    double precise_num = 3.146;
    double multiplier = 100.0;
    double rounded = std::round(precise_num * multiplier) / multiplier;

    std::cout << "rounded to two decimals: " << rounded << "\n"; // פלט: rounded to two decimals: 3.15

    return 0;
}
```

## עיון מעמיק
לפני C++11, העיגול התבסס על טכניקות ידניות או ספריות לא סטנדרטיות. היום, `<cmath>` מספקת שיטות חזקות. `floor()` עוגל למטה, `ceil()` עוגל למעלה, בעוד `round()` עובר לשלם הקרוב ביותר, תוך טיפול גם במקרי השוויון (מקרים של 0.5) על ידי עיגול למספר הזוגי.

הבנת הת comportה של פונקציות אלו קריטית; למשל, מספרים שליליים עלולים להפתיע (`std::round(-2.5)` מחזיר `-2.0`).

אלטרנטיבות? הטלה ל-int לאחר הוספת 0.5 למספרים חיוביים הייתה טריק קלאסי אך טועה עם שליליים ואינה תלויה בסוג. ספריות כמו Boost יכולות להציע גישות מורכבות יותר, בעוד שרחבות שפה או פונקציות פנימיות של מהדר יכולות להתאים אופטימיזציה לחומרה ספציפית.

## ראו גם
- הפניה ל-C++ עבור `<cmath>`: https://en.cppreference.com/w/cpp/header/cmath
- התקן IEEE לחישוב נקודה צפה (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- ספריית המרה מספרית של Boost: https://www.boost.org/doc/libs/release/libs/numeric/conversion/
