---
title:                "עיגול מספרים"
aliases:
- /he/cpp/rounding-numbers.md
date:                  2024-01-26T03:43:39.458849-07:00
model:                 gpt-4-0125-preview
simple_title:         "עיגול מספרים"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/rounding-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
עיגול מספרים משמעו לשנות ערך לשכן השלם הקרוב שלו או לדיוק מצוין. מפתחים עושים זאת כדי לפשט, להתאים למגבלות העולם האמיתי או לשפר ביצועים על ידי השמטת דיוק עודף.

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
