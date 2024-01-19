---
title:                "חילוץ תת-מחרוזות"
html_title:           "Bash: חילוץ תת-מחרוזות"
simple_title:         "חילוץ תת-מחרוזות"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## מה ולמה?

חילוץ תת-מחרוזות (substring) הוא פעולה של קיצוז מחרוזת למקטע קטן יותר. מתכנתים משתמשים בכך למען מעקב אחר מידע ספציפי במחרוזת או לתהליך מידע בצורה מקוטלת.

## איך?

באמצעות הפונקציה `substr()` ב-C++, ניתן לחלץ תת-מחרוזת. ראה את הדוגמאות הבאות:

```C++
#include <iostream>
#include <string>

int main() {
    std::string str = "Hello, World!";
    std::string substr = str.substr(7, 5);
     
    std::cout << substr;
     
    return 0;
}
```
פלט:
```
World
```

## צוללים לפרטים

אזלנו נקודה היסטורית: הפונקציה `substr()` הוצגה בתחילה במימד בינלאומי של C++ (ISO) בשנת 1990 . מאז, היא הפכה לדרך נפוצה במיוחד לראות במחרוזת.

בניגוד לפונקציה `substr()`, C++ גם כוללת את התוספות `find_first_of()` ו- `find_last_of()`. אלו מאפשרים למתכנתים לחפש סוף ותחילת תת-מחרוזת.

היא גם מיישמת את substring על ידי שימוש בשני מידעים – האינדקס שבו אנו רוצים להתחיל והאורך של התת-מחרוזת. 

## ראו גם

1. [C++ string substr() - w3schools](https://www.w3schools.com/cpp/cpp_string_substr.asp)
2. [C++ substr() Function - tutorialspoint](https://www.tutorialspoint.com/cplusplus/cpp_string_substr.htm)
3. [Working With String Substrings in C++ - programiz](https://www.programiz.com/cpp-programming/library-function/cctype/substr)