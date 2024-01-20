---
title:                "הגדלת אותיות של מחרוזת"
html_title:           "C++: הגדלת אותיות של מחרוזת"
simple_title:         "הגדלת אותיות של מחרוזת"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
קיבוץ של מחרוזת מתייחס לפעולה של שינוי כל אות במחרוזת לאות גדולה. מתכנתים עשויים לקבץ מחרוזות כדי להפוך את הקלט מאותיות קטנות לגדולות או לאחידה של מחרוזות למטרות שליטה.

## איך לעשות:
הנה את קוד ה-C++ שיאפשר לך לקבץ מחרודת:

```C++
#include <algorithm>
#include <cctype>
#include <iostream>
#include <string>

int main() {
    std::string s = "hello, world!";
    std::transform(s.begin(), s.end(), s.begin(),
                   [](unsigned char c){ return std::toupper(c); });

    std::cout << s;
}
```

התוצאה שתודפס תהיה:
```C++
HELLO, WORLD!
```

## שוליים 
קיבוץ של מחרוזות הוא שיטה שעשויה להשתנות בין שפות תכנות שונות. האלטרנטיבה העיקרית לקיבוץ ב-C++ היא לעבור על כל האותיות בצורה אישית ולשנותם מאותיות קטנות לגדולות.
הפונקציה std::toupper() ממיימה תהליך אישי שבו היא משתמשת ב-ASCII values של האותיות כדי לשנותם לאותיות גדולות.

## ראה גם 
* [דף המפרט הרשמי של std::toupper()](https://en.cppreference.com/w/cpp/string/byte/toupper)
* [דף המפרט הרשמי של std::transform()](https://en.cppreference.com/w/cpp/algorithm/transform)