---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "Go: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
המרת מחרוזת לאותיות קטנות היא תהליך שבו אנו ממירים את כל האותיות גדולות במחרוזת לאותיות קטנות. התכנתים עושים זאת כדי להימנע מבעיות שלא מצריכות את ההבנה, כמו תנאים שלא מצריכים רגישות לאותיות גדולות וקטנות.

## איך לעשות את זה:
המרת מחרוזת לאותיות קטנות ב- C++ נעשה באמצעות פונקציה שנקראת `std::transform`. הנה כיצד נראה זה בקוד:

```C++
#include <algorithm>
#include <string>
#include <iostream>

int main() {
    std::string s = "Hello, World!";
    std::transform(s.begin(), s.end(), s.begin(),
                   [](unsigned char c){ return std::tolower(c); }
                  );
    std::cout << s << std::endl;
    return 0;
}
```

בפלט תקבל "hello, world!".

## מעמיקים יותר
הגישה של `std::transform` אל מחרוזות התפתחה ממשפחת הפונקציות של STL ב- C++ שנוצרו מדי STL - הספריה התקנית של תבניות. אף על פי שיש גם אפשרויות אחרות כמו `boost::to_lower`, `std::transform` אל תלמדית יותר ודי מסתדרת בצורה יעילה עם מחרוזות ASCII.

## ראו גם
ניתן למצוא מידע נוסף בנושאים קשורים בקישורים הבאים:
- [תיעוד std::transform](https://en.cppreference.com/w/cpp/algorithm/transform)
- [תיעוד std::tolower](https://en.cppreference.com/w/cpp/string/byte/tolower)
- [תיעוד Boost](https://www.boost.org/)