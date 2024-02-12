---
title:                "חילוץ תת-מחרוזות"
aliases:
- he/cpp/extracting-substrings.md
date:                  2024-01-20T17:45:32.730753-07:00
model:                 gpt-4-1106-preview
simple_title:         "חילוץ תת-מחרוזות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/extracting-substrings.md"
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
