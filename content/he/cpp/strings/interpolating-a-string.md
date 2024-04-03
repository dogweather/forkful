---
date: 2024-01-20 17:50:55.893590-07:00
description: "\u05E4\u05D5\u05DC\u05DE\u05D5\u05E1 \u05E8\u05E6\u05E4\u05D9\u05DD\
  \ \u05D4\u05D5\u05D0 \u05D4\u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05D0\
  \u05E0\u05D7\u05E0\u05D5 \u05DE\u05D5\u05E1\u05D9\u05E4\u05D9\u05DD \u05DE\u05E9\
  \u05EA\u05E0\u05D9\u05DD \u05D0\u05D5 \u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD\
  \ \u05DC\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA. \u05D6\u05D4 \u05E0\
  \u05E2\u05E9\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D9\u05E6\u05D5\u05E8 \u05D4\u05D5\
  \u05D3\u05E2\u05D5\u05EA \u05DE\u05D5\u05EA\u05D0\u05DE\u05D5\u05EA \u05D0\u05D9\
  \u05E9\u05D9\u05EA \u05D0\u05D5 \u05E4\u05DC\u05D8 \u05D3\u05D9\u05E0\u05DE\u05D9\
  ."
lastmod: '2024-03-13T22:44:39.813018-06:00'
model: gpt-4-1106-preview
summary: "\u05E4\u05D5\u05DC\u05DE\u05D5\u05E1 \u05E8\u05E6\u05E4\u05D9\u05DD \u05D4\
  \u05D5\u05D0 \u05D4\u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05D1\u05D5 \u05D0\u05E0\
  \u05D7\u05E0\u05D5 \u05DE\u05D5\u05E1\u05D9\u05E4\u05D9\u05DD \u05DE\u05E9\u05EA\
  \u05E0\u05D9\u05DD \u05D0\u05D5 \u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05DC\
  \u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA."
title: "\u05E9\u05E8\u05D1\u05D5\u05D1 \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 8
---

## How to: (איך לעשות:)
ב-C++ עדיין אין תמיכה רשמית בפולמוס רצפים כמו בשפות אחרות (למשל, Python או JavaScript). אבל אנחנו יכולים עדיין להשיג תוצאות דומות. הנה דוגמה:

```C++
#include <iostream>
#include <string>

int main() {
    std::string name = "יוסי";
    int age = 30;

    // שימוש ב-operator+ לשילוב מחרוזות ומשתנים
    std::string greeting = "שלום, " + name + "! אתה בן " + std::to_string(age) + " שנים.";
    std::cout << greeting << std::endl;

    return 0;
}
```
תוצאת דוגמה:
```
שלום, יוסי! אתה בן 30 שנים.
```

## Deep Dive (עיון מעמיק)
למרות שב-C++ אין פונקציה קסומה לפולמוס רצפים, ניתן להשתמש במתודות כמו `std::stringstream` או פונקציות סידור מחרוזות כמו `sprintf` (אבל עם זהירות גדולה כדי למנוע באגים או נקודות תקלה באבטחה). כן הייתה ניסיון להוסיף את fmtlib, ספריה שפותחת דרך מודרנית יותר לעשות פולמוס רצפים, לתקן ה-C++20 אבל היא לא הספיקה להתקבל בזמן.

## See Also (ראה גם)
- [fmtlib](https://github.com/fmtlib/fmt): ספריה חיצונית המספקת יכולת פולמוס רצפים מודרנית.
- [C++ reference for std::stringstream](https://en.cppreference.com/w/cpp/io/basic_stringstream): מידע על `std::stringstream` במדריך ה-C++ הרשמי.
- [C++ reference for std::to_string](https://en.cppreference.com/w/cpp/string/basic_string/to_string): מידע נוסף על המרת מספרים למחרוזות ב-C++.
