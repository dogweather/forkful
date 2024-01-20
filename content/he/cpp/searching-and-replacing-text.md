---
title:                "חיפוש והחלפת טקסט"
html_title:           "Elm: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מה ולמה?
חיפוש והחלפת טקסט היא תהליך שבו אנחנו מחפשים מחרוזת תווים מסוימת ומחליפים אותה במחרוזת אחרת. לתכנתים זה נחוץ לעיבוד טקסטים, הקלה של שינויי קוד, ובצורה כללית, כל מקרה שבו המידע משתנה.

## איך לעשות:
אנחנו יכולים להשתמש בפונקציה replace של הספרייה string ב- C++. הקוד יראה כך:

```C++
#include <iostream>
#include <string>

int main() {
    std::string s = "Hello, World!";
    std::string old_val = "World";
    std::string new_val = "C++";
    size_t pos = s.find(old_val);

    if (pos != std::string::npos)
        s.replace(pos, old_val.length(), new_val);

    std::cout << s << std::endl;

    return 0;
}
```
הפלט של הקוד הזה יהיה: "Hello, C++!"

## צלילה עמוקה
חיפוש והחלפת טקסט בשפות תכנות הוא אספקט חשוב מאוד שהשתנה במהלך השנים. גרסאות מוקדמות של שפות תכנות אפשרו רק חיפושים פשוטים, אך בהמשך התפתחו שפות תכנות מתקדמות יותר כמו C++ שמאפשרות פעולות מורכבות יותר. 

חלופות לפונקציה replace של string הן תוספות כמו Boost (boost::algorithm::replace_all, עבור דוגמה) או שימוש ברגולרי אקספרשן. 

חשוב לזכור שפונקציה replace מבצעת חיפוש והחלפה היכן שהמחרוזת המועתקת מתחילה ומסתיימת, ולא מחפשת את המחרוזת בכל הטקסט.

## ראו גם
1. [מדריך Boost string algorithms library](https://www.boost.org/doc/libs/1_76_0/doc/html/string_algo.html)
2. [מדריך פעולות מחרוזת ב-C++](http://www.cplusplus.com/reference/string/string/replace/)
3. [מדריך לרגולרי אקספרשן](https://regexone.com/)