---
title:                "שרבוב מחרוזת"
date:                  2024-01-20T17:50:55.893590-07:00
model:                 gpt-4-1106-preview
simple_title:         "שרבוב מחרוזת"

category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
פולמוס רצפים הוא התהליך שבו אנחנו מוסיפים משתנים או ביטויים לתוך מחרוזת. זה נעשה כדי ליצור הודעות מותאמות אישית או פלט דינמי.

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
