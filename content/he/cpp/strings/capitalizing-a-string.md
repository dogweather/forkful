---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:36.786957-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-C++, \u05E0\
  \u05D9\u05EA\u05DF \u05DC\u05D4\u05D2\u05D3\u05D9\u05DC \u05D0\u05D5\u05EA \u05E8\
  \u05D0\u05E9\u05D5\u05E0\u05D4 \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D1\
  \u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D4\u05E1\u05E4\u05E8\u05D9\u05D9\u05D4\
  \ \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\u05EA \u05DC\u05DC\u05D0 \u05E6\
  \u05D5\u05E8\u05DA \u05D1\u05E1\u05E4\u05E8\u05D9\u05D5\u05EA \u05E6\u05D3 \u05E9\
  \u05DC\u05D9\u05E9\u05D9. \u05E2\u05DD \u05D6\u05D0\u05EA, \u05E2\u05D1\u05D5\u05E8\
  \ \u05D4\u05EA\u05E0\u05D4\u05D2\u05D5\u05D9\u05D5\u05EA \u05D4\u05D2\u05D3\u05DC\
  \u05D4 \u05DE\u05D5\u05E8\u05DB\u05D1\u05D5\u05EA \u05D0\u05D5 \u05E1\u05E4\u05E6\
  \u05D9\u05E4\u05D9\u05D5\u05EA\u2026"
lastmod: '2024-03-13T22:44:39.808237-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-C++, \u05E0\u05D9\u05EA\u05DF \u05DC\u05D4\u05D2\u05D3\u05D9\u05DC\
  \ \u05D0\u05D5\u05EA \u05E8\u05D0\u05E9\u05D5\u05E0\u05D4 \u05D1\u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D4\u05E1\u05E4\
  \u05E8\u05D9\u05D9\u05D4 \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\u05EA\
  \ \u05DC\u05DC\u05D0 \u05E6\u05D5\u05E8\u05DA \u05D1\u05E1\u05E4\u05E8\u05D9\u05D5\
  \u05EA \u05E6\u05D3 \u05E9\u05DC\u05D9\u05E9\u05D9."
title: "\u05D4\u05D2\u05D3\u05DC\u05EA \u05D0\u05D5\u05EA\u05D9\u05D5\u05EA \u05D1\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 2
---

## איך לעשות:
ב-C++, ניתן להגדיל אות ראשונה במחרוזת באמצעות הספרייה הסטנדרטית ללא צורך בספריות צד שלישי. עם זאת, עבור התנהגויות הגדלה מורכבות או ספציפיות יותר, ספריות כמו Boost יכולות להיות מועילות מאוד. להלן דוגמאות הממחישות את שתי הגישות.

### באמצעות ספריית C++ סטנדרטית:
```cpp
#include <iostream>
#include <cctype> // עבור std::tolower ו-std::toupper
#include <string>

std::string capitalizeString(const std::string& input) {
    std::string result;
    bool capitalizeNext = true;

    for (char ch : input) {
        if (std::isspace(ch)) {
            capitalizeNext = true;
        } else if (capitalizeNext) {
            ch = std::toupper(ch);
            capitalizeNext = false;
        }
        result += ch;
    }

    return result;
}

int main() {
    std::string text = "hello world from c++";
    std::string capitalizedText = capitalizeString(text);
    std::cout << capitalizedText << std::endl; // פלט: "Hello World From C++"
}
```

### באמצעות ספריית Boost:
למניפולציה מתקדמת של מחרוזות, כולל הגדלת אות ראשונה תוך התחשבות באזור, ייתכן שתרצה להשתמש בספריית Boost String Algo.

ראשית, הבטח שהספרייה של Boost מותקנת ומוגדרת בפרויקט שלך. לאחר מכן תוכל לכלול את הכותרות הנדרשות ולהשתמש בתכונות שלה כפי שמוצג למטה.

```cpp
#include <boost/algorithm/string.hpp>
#include <iostream>
#include <string>

int main() {
    std::string text = "hello world from c++";
    std::string capitalizedText = text;

    // הגדלת אות ראשונה של כל מילה
    boost::algorithm::to_lower(capitalizedText); // הבטחת שהמחרוזת באותיות קטנות
    capitalizedText[0] = std::toupper(capitalizedText[0]); // הגדלת התו הראשון

    for (std::size_t i = 1; i < capitalizedText.length(); ++i) {
        if (isspace(capitalizedText[i - 1])) { // הגדלה לאחר רווח
            capitalizedText[i] = std::toupper(capitalizedText[i]);
        }
    }

    std::cout << capitalizedText << std::endl; // פלט: "Hello World From C++"
}
```

במקרה זה, Boost מפשטת חלק ממשימות מניפולציית המחרוזת אך עדיין דורשת גישה מותאמת אישית להגדלה אמיתית מכיוון שהיא בעיקר מציעה פונקציונליות להמרה והעברה בין רישיות.
