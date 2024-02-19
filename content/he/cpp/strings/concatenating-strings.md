---
aliases:
- /he/cpp/concatenating-strings/
date: 2024-01-20 17:34:22.914444-07:00
description: "\u05D4\u05E6\u05DE\u05D3\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA\
  \ (Concatenating Strings) \u05D4\u05D9\u05D0 \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\
  \u05D1\u05D5 \u05DE\u05D7\u05D1\u05E8\u05D9\u05DD \u05E9\u05EA\u05D9 \u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05D5\u05EA \u05D0\u05D5 \u05D9\u05D5\u05EA\u05E8 \u05DC\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05D0\u05D7\u05EA. \u05DE\u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05DE\u05D1\u05E6\u05E2\u05D9\u05DD \u05D4\u05E6\u05DE\u05D3\u05D4\
  \ \u05DB\u05D3\u05D9 \u05DC\u05D9\u05E6\u05D5\u05E8 \u05DE\u05E9\u05E4\u05D8\u05D9\
  \u05DD, \u05D4\u05D5\u05D3\u05E2\u05D5\u05EA \u05D5\u05EA\u05E6\u05D5\u05E8\u05D5\
  \u05EA \u05D8\u05E7\u05E1\u05D8\u2026"
lastmod: 2024-02-18 23:08:53.149744
model: gpt-4-1106-preview
summary: "\u05D4\u05E6\u05DE\u05D3\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA\
  \ (Concatenating Strings) \u05D4\u05D9\u05D0 \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\
  \u05D1\u05D5 \u05DE\u05D7\u05D1\u05E8\u05D9\u05DD \u05E9\u05EA\u05D9 \u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05D5\u05EA \u05D0\u05D5 \u05D9\u05D5\u05EA\u05E8 \u05DC\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05D0\u05D7\u05EA. \u05DE\u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05DE\u05D1\u05E6\u05E2\u05D9\u05DD \u05D4\u05E6\u05DE\u05D3\u05D4\
  \ \u05DB\u05D3\u05D9 \u05DC\u05D9\u05E6\u05D5\u05E8 \u05DE\u05E9\u05E4\u05D8\u05D9\
  \u05DD, \u05D4\u05D5\u05D3\u05E2\u05D5\u05EA \u05D5\u05EA\u05E6\u05D5\u05E8\u05D5\
  \u05EA \u05D8\u05E7\u05E1\u05D8\u2026"
title: "\u05E9\u05E8\u05E9\u05D5\u05E8 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
הצמדת מחרוזות (Concatenating Strings) היא תהליך שבו מחברים שתי מחרוזות או יותר למחרוזת אחת. מתכנתים מבצעים הצמדה כדי ליצור משפטים, הודעות ותצורות טקסט מורכבות.

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
