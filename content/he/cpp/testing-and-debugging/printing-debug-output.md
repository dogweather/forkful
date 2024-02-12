---
title:                "הדפסת פלט לניפוי באגים"
aliases:
- /he/cpp/printing-debug-output.md
date:                  2024-01-20T17:52:54.912428-07:00
model:                 gpt-4-1106-preview
simple_title:         "הדפסת פלט לניפוי באגים"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## מה ולמה?
פלט דיבאג הוא כלי שמפרסם מידע אודות תקלות אפשריות בקוד. תכנתים משתמשים בזה לאבחן באגים ולהבין את הפלואו של התוכנית.

## איך לעשות:
```C++
#include <iostream>

int main() {
    // דוגמא להדפסת דיבאג בסיסית
    int x = 10;
    std::cerr << "Debug: ערך של x הוא " << x << std::endl;

    // כאשר הבעיה מתרחשת
    if(x > 5) {
        std::cerr << "Debug: x גדול מ-5" << std::endl;
    }

    // להדפסת השגיאות או בעיות
    try {
        throw std::runtime_error("שגיאה: משהו השתבש");
    } catch(const std::exception& e) {
        std::cerr << "Exception Caught: " << e.what() << std::endl;
    }

    return 0;
}

// דוגמא לפלט:
// Debug: ערך של x הוא 10
// Debug: x גדול מ-5
// Exception Caught: שגיאה: משהו השתבש
```

## עיון יסודי
בתקופות ראשונות, הדפסת דיבאג נעשתה לעיתים באמצעות הדפסה על נייר. כיום, הפקת פלטים כמו `std::cerr` ב-C++ או פקודות דומות בשפות אחרות שימושיות במיוחד עבור מעקב אחר ואיתור בעיות. חלופות כוללות מערכות לוגינג מתוחכמות, סביבות פיתוח משוכללות (IDEs) עם כלים מובנים לניפוי באגים, ושימוש במאזינים (listeners) וב-assertions. ניתן גם להדפיס לקובץ במקום לקונסול, דבר שמאפשר ניתוח לאחר מכן.

## ראו גם
- [cppreference.com](https://en.cppreference.com/w/cpp/io/cerr): מדריך לאובייקט `std::cerr` ב-CPPReference.
- [learn-cpp.org](https://www.learn-cpp.org/): מקורות לימוד ל-C++.
- [GitHub: Awesome C++](https://github.com/fffaraz/awesome-cpp): קישור לאוסף של מקורות מומלצים, ספריות וכלים ל-C++.
