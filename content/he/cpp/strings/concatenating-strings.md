---
date: 2024-01-20 17:34:22.914444-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-C++ \u05D9\
  \u05E9 \u05DE\u05E1\u05E4\u05E8 \u05D3\u05E8\u05DB\u05D9\u05DD \u05DC\u05D1\u05E6\
  \u05E2 \u05D4\u05E6\u05DE\u05D3\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA\
  . \u05D4\u05E0\u05D4 \u05DB\u05DE\u05D4 \u05D3\u05D5\u05D2\u05DE\u05D0\u05D5\u05EA\
  ."
lastmod: '2024-03-13T22:44:39.822182-06:00'
model: gpt-4-1106-preview
summary: "\u05D1-C++ \u05D9\u05E9 \u05DE\u05E1\u05E4\u05E8 \u05D3\u05E8\u05DB\u05D9\
  \u05DD \u05DC\u05D1\u05E6\u05E2 \u05D4\u05E6\u05DE\u05D3\u05EA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05D5\u05EA."
title: "\u05E9\u05E8\u05E9\u05D5\u05E8 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA"
weight: 3
---

## איך לעשות:
ב-C++ יש מספר דרכים לבצע הצמדת מחרוזות. הנה כמה דוגמאות:

```cpp
#include <iostream>
#include <string>

int main() {
    // הצמדה באמצעות אופרטור ה+
    std::string firstName = "אליהו";
    std::string lastName = "גולדראט";
    std::string fullName = firstName + " " + lastName;
    std::cout << fullName << std::endl; // אליהו גולדראט

    // הצמדה באמצעות .append()
    std::string greeting = "שלום ";
    greeting.append(firstName);
    std::cout << greeting << std::endl; // שלום אליהו

    // הצמדה עם משתנים של סוגים שונים
    int day = 15;
    std::string date = "אפריל " + std::to_string(day);
    std::cout << date << std::endl; // אפריל 15

    return 0;
}
```

## עיון מעמיק:
מחרוזות היו תמיד חלק מרכזי בתכנות, והצמדתן - טכניקה יסודית. בהיסטוריה, שפות כמו C דרשו פונקציות כמו `strcat()` להצמדת מחרוזות. ב-C++, `std::string` מספקת פתרונות רבים ויעילים יותר.

חלופות נפוצות להצמדה כוללות שימוש ב-string streams או בפונקציות של טיפול בעיבוד מחרוזות מסודרות, כמו גרסאות של `sprintf()`.

הביצועים של הצמדת מחרוזות עלולים להיות גרועים אם לא נעשים בצורה אופטימלית, במיוחד בלולאות ארוכות או עבור מחרוזות גדולות מאוד, כיוון שכל הצמדה יוצרת עותק חדש של המחרוזת.

## קרא גם:
- [אתר cplusplus על std::string](http://www.cplusplus.com/reference/string/string/)
- [תיעוד של C++ על הצמדת מחרוזות](https://en.cppreference.com/w/cpp/string/basic_string/operator%2B)
- [רפרנס של טכניקות השפה](http://cppreference.com/)
