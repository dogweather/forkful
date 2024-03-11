---
date: 2024-01-20 17:45:32.730753-07:00
description: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05D5\u05EA \u05D6\u05D4 \u05D0\u05D5\u05DE\u05E8 \u05DC\u05E7\u05D7\u05EA\
  \ \u05D7\u05DC\u05E7 \u05DE\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05E7\u05D9\u05D9\
  \u05DE\u05EA. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD\
  \ \u05D0\u05EA \u05D6\u05D4 \u05DB\u05E9\u05D4\u05DD \u05E6\u05E8\u05D9\u05DB\u05D9\
  \u05DD \u05E8\u05E7 \u05E7\u05D8\u05E2 \u05DE\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  \ \u05DC\u05D8\u05D9\u05E4\u05D5\u05DC \u05E0\u05E4\u05E8\u05D3 \u05D0\u05D5 \u05D0\
  \u05E0\u05DC\u05D9\u05D6\u05D4."
lastmod: '2024-03-11T00:14:13.307149-06:00'
model: gpt-4-1106-preview
summary: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA \u05D6\u05D4 \u05D0\u05D5\u05DE\u05E8 \u05DC\u05E7\u05D7\u05EA \u05D7\
  \u05DC\u05E7 \u05DE\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05E7\u05D9\u05D9\u05DE\
  \u05EA. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D0\
  \u05EA \u05D6\u05D4 \u05DB\u05E9\u05D4\u05DD \u05E6\u05E8\u05D9\u05DB\u05D9\u05DD\
  \ \u05E8\u05E7 \u05E7\u05D8\u05E2 \u05DE\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DC\
  \u05D8\u05D9\u05E4\u05D5\u05DC \u05E0\u05E4\u05E8\u05D3 \u05D0\u05D5 \u05D0\u05E0\
  \u05DC\u05D9\u05D6\u05D4."
title: "\u05D7\u05D9\u05DC\u05D5\u05E5 \u05EA\u05EA-\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05D5\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
חילוץ תת-מחרוזות זה אומר לקחת חלק ממחרוזת קיימת. תכנתים עושים את זה כשהם צריכים רק קטע ממחרוזת לטיפול נפרד או אנליזה.

## איך לעשות:
ב-C++ יש כמה דרכים לחלץ תת-מחרוזות. נתמקד בשיטה הכי פשוטה - `substr()`.

```C++
#include <iostream>
#include <string>

int main() {
    std::string fullStr = "שלום עולם";
    std::string subStr = fullStr.substr(0, 4); // חולץ את "שלום"

    std::cout << subStr << std::endl; // ידפיס "שלום"

    return 0;
}
```
תוצאת הדפסה: שלום

## עיון מעמיק:
חילוץ תת-מחרוזות הוא תכנות לחם וחמאה מימי קדם. כבר בשפות תכנות מוקדמות הייתה התמכרות לפונקציונליות זו.
ב-C++, השיטה `substr()` הגיעה עם תקן ה-Standard Template Library (STL). יש גם חלופות, כמו לעבור על המחרוזת בעצמך או להשתמש ב-iterators או ב-regex לחילוץ יותר מתוחכם.
בעיות נפוצות בחילוץ תת-מחרוזות כוללות index out of bounds או קודינג שגוי של תווים ייחודיים, כמו מחרוזות בעברית.

## ראו גם:
- [std::string::substr](https://en.cppreference.com/w/cpp/string/basic_string/substr) - מידע מעמיק על הפונקציה `substr`.
- [cppreference.com](https://cppreference.com/) - מקור מקיף לתיעוד על C++.
- [Regular Expressions in C++](https://www.cplusplus.com/reference/regex/) - כיצד להשתמש בביטויים רגולריים כדי למצוא ולחלץ תת-מחרוזות.
