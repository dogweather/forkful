---
title:                "המרת מחרוזת לאותיות קטנות"
aliases:
- /he/cpp/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:38:34.010010-07:00
model:                 gpt-4-1106-preview
simple_title:         "המרת מחרוזת לאותיות קטנות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מה ולמה?
המרת מחרוזת לאותיות קטנות היא תהליך שבו כל האותיות הגדולות במחרוזת משתנות לאותיות קטנות. זה נעשה לעיתים קרובות כדי להבטיח אחידות בטיפול בטקסט ולערוך השוואות מחרוזות ללא תלות ברישיות.

## איך לעשות:
בדוגמה הבאה נשתמש בפונקציה `std::transform` כדי להמיר כל אות במחרוזת לאות קטנה.

```C++
#include <iostream>
#include <algorithm>
#include <string>
#include <cctype> // לשימוש בפונקציה tolower

int main() {
    std::string text = "HeLLo WoRLD!";
    std::transform(text.begin(), text.end(), text.begin(), 
                   [](unsigned char c) -> unsigned char { return std::tolower(c); });

    std::cout << text; // פלט: hello world!
    return 0;
}
```

## מבט עמוק:
היסטורית, המרה לאותיות קטנות התבצעה גם בעזרת לולאות שרצו על כל תו במחרוזת וביצעו השוואות והמרות ידניות. כלי הסטנדרטי `std::transform` הגיע לפשט את התהליך הזה. חלופות נוספות כוללות שימוש בפונקציות כמו `tolower` בצורה ישירה בתוך לולאה או שימוש בספריות שלישיות שמוסיפות יכולות מניפולציה על מחרוזות.

גם כאשר משתמשים ב`std::tolower`, חשוב להעביר את הפרמטר כ-`unsigned char` כדי למנוע בעיות עם תווים שאינם חלק מ-ASCII. עם זאת, כשעובדים עם UTF-8 או מערכת תווים אחרת, עלולות להיות דרישות מיוחדות שצריך לתת להן את הדעת.

## ראה גם:
- Documentation on `std::transform`: https://en.cppreference.com/w/cpp/algorithm/transform
- Documentation on `std::tolower`: https://en.cppreference.com/w/cpp/string/byte/tolower
- C++ reference for `std::string`: https://en.cppreference.com/w/cpp/string/basic_string
- Unicode handling in C++: https://unicode-org.github.io/icu/userguide/cpp/
