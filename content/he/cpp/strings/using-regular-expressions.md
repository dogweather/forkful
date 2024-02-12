---
title:                "שימוש בביטויים רגולריים"
date:                  2024-02-03T19:16:36.358693-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש בביטויים רגולריים"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
ביטויים רגולריים ב-C++ הם סדרות של תווים המגדירות תבנית חיפוש, המשמשות להתאמת מחרוזות או למניפולציה עליהן. מתכנתים משתמשים בהם למשימות כמו אימות קלט, חיפוש אחר מופעים בתוך מחרוזות, או פיצול מחרוזות לאסימונים, מה שהופך אותם לכלי בלתי נפרד לעיבוד טקסט יעיל ואפקטיבי.

## איך לעשות:
C++11 הוסיף תמיכה בביטויים רגולריים בספריית התקן, `<regex>‏`, המציעה מסגרת עמידה לחיפושים ומניפולציות על מחרוזות. הנה דוגמה בסיסית לשימוש בביטויים רגולריים לחיפוש תבנית בתוך מחרוזת:

```cpp
#include <iostream>
#include <regex>

int main() {
    std::string target = "Hello, my email is example@example.com";
    std::regex email_pattern(R"(\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}\b)");

    if (std::regex_search(target, email_pattern)) {
        std::cout << "Email found!" << std::endl;
    } else {
        std::cout << "No email found." << std::endl;
    }

    return 0;
}
```
**דוגמת פלט**
```
Email found!
```

למניפולציות מורכבות יותר, כמו החלפת תבניות בתוך מחרוזות, ביטויים רגולריים של C++ יכולים להיות מאוד שימושיים:

```cpp
#include <iostream>
#include <regex>

int main() {
    std::string text = "The rain in Spain falls mainly in the plain.";
    std::regex vowel_regex("([aeiou])");

    std::string replaced_text = std::regex_replace(text, vowel_regex, "*");
    std::cout << replaced_text << std::endl;

    return 0;
}
```
**דוגמת פלט**
```
Th* r**n *n Sp**n f*lls m**nly *n th* pl**n.
```

למתכנתים המחפשים אופציות מחוץ לספריית התקן, ספריית ה-Boost Regex (`boost/regex.hpp`) היא אפשרות פופולרית מבין צד שלישי המציעה יכולות ביטוי רגולרי מתקדמות ואופטימיזציות ביצועים, במיוחד לתבניות מורכבות או עיבוד נתונים רחב היקף:

```cpp
#include <iostream>
#include <boost/regex.hpp>

int main() {
    std::string s = "Boost libraries are fun!";
    boost::regex expr("(\\w+)\\s(libraries)"); // תואם "Boost libraries"
    std::string fmt("GNU \\1"); // מחליף ל-"GNU Boost"

    std::string result = boost::regex_replace(s, expr, fmt);
    std::cout << result << std::endl;

    return 0;
}
```
**דוגמת פלט**
```
GNU Boost are fun!
```

דוגמאות אלו מציגות רק חלק מהיכולות של C++ עם ביטויים רגולריים, מדגימות חיפושים בסיסיים, התאמת תבניות והחלפות, בין אם באמצעות ספריית התקן או בהגברה באמצעות המימוש העוצמתי של ביטוי רגולרי של Boost.
