---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:36.358693-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: C++11 \u05D4\u05D5\
  \u05E1\u05D9\u05E3 \u05EA\u05DE\u05D9\u05DB\u05D4 \u05D1\u05D1\u05D9\u05D8\u05D5\
  \u05D9\u05D9\u05DD \u05E8\u05D2\u05D5\u05DC\u05E8\u05D9\u05D9\u05DD \u05D1\u05E1\
  \u05E4\u05E8\u05D9\u05D9\u05EA \u05D4\u05EA\u05E7\u05DF, `<regex>\u200F`, \u05D4\
  \u05DE\u05E6\u05D9\u05E2\u05D4 \u05DE\u05E1\u05D2\u05E8\u05EA \u05E2\u05DE\u05D9\
  \u05D3\u05D4 \u05DC\u05D7\u05D9\u05E4\u05D5\u05E9\u05D9\u05DD \u05D5\u05DE\u05E0\
  \u05D9\u05E4\u05D5\u05DC\u05E6\u05D9\u05D5\u05EA \u05E2\u05DC \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05D5\u05EA. \u05D4\u05E0\u05D4 \u05D3\u05D5\u05D2\u05DE\u05D4 \u05D1\
  \u05E1\u05D9\u05E1\u05D9\u05EA \u05DC\u05E9\u05D9\u05DE\u05D5\u05E9\u2026"
lastmod: '2024-03-13T22:44:39.819156-06:00'
model: gpt-4-0125-preview
summary: "C++11 \u05D4\u05D5\u05E1\u05D9\u05E3 \u05EA\u05DE\u05D9\u05DB\u05D4 \u05D1\
  \u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD \u05E8\u05D2\u05D5\u05DC\u05E8\u05D9\u05D9\
  \u05DD \u05D1\u05E1\u05E4\u05E8\u05D9\u05D9\u05EA \u05D4\u05EA\u05E7\u05DF, `<regex>\u200F\
  `, \u05D4\u05DE\u05E6\u05D9\u05E2\u05D4 \u05DE\u05E1\u05D2\u05E8\u05EA \u05E2\u05DE\
  \u05D9\u05D3\u05D4 \u05DC\u05D7\u05D9\u05E4\u05D5\u05E9\u05D9\u05DD \u05D5\u05DE\
  \u05E0\u05D9\u05E4\u05D5\u05DC\u05E6\u05D9\u05D5\u05EA \u05E2\u05DC \u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05D5\u05EA."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05D1\u05D9\u05D8\u05D5\u05D9\u05D9\u05DD\
  \ \u05E8\u05D2\u05D5\u05DC\u05E8\u05D9\u05D9\u05DD"
weight: 11
---

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
