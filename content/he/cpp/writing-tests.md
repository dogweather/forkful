---
title:                "כתיבת בדיקות"
html_title:           "Bash: כתיבת בדיקות"
simple_title:         "כתיבת בדיקות"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבת בדיקות זה לבנות תרחישים שבודקים את התוכנה שלך. תוכניתאים עושים את זה כדי לוודא שהקוד עובד כמו שצריך, לזהות באגים מוקדם, ולחזק את הביטחון באיכות המוצר.

## איך לעשות:
```C++
#include <iostream>
#include <cassert>

int Add(int a, int b) {
    return a + b;
}

int main() {
    assert(Add(2, 2) == 4);
    std::cout << "Test passed: 2 + 2 equals " << Add(2, 2) << std::endl;
    
    assert(Add(-1, -1) == -2);
    std::cout << "Test passed: -1 + (-1) equals " << Add(-1, -1) << std::endl;
    
    // תוכל להוסיף עוד בדיקות כאן
    
    return 0;
}
```
הרצת הקוד הזה תדפיס:
```
Test passed: 2 + 2 equals 4
Test passed: -1 + (-1) equals -2
```

## ניתוח עמוק
הרעיון שמאחורי בדיקות הוא ישן ככתיבת התוכנה עצמה. חלופות ל-`assert` כוללות מערכות בדיקה מורכבות יותר כמו Google Test או Catch2. הן מאפשרות לזהות יותר סוגי בדיקות, להפריד בין בדיקות, ולקבל פלט מפורט יותר כאשר משהו לא עובד.

## ראה גם
- [Google Test](https://github.com/google/googletest)
- [Catch2](https://github.com/catchorg/Catch2)
