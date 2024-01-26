---
title:                "כתיבת בדיקות"
html_title:           "Bash: כתיבת בדיקות"
simple_title:         "כתיבת בדיקות"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/c/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
כתיבת בדיקות זה תהליך שבו מנסחים מקרי ניסיון כדי לוודא את תקינות הקוד. מתכנתים עושים זאת כדי לקלוט באגים מוקדם ולשמור על איכות התוכנה.

## How to: (איך לעשות:)
בסיסי:
```C
#include <assert.h>

void addTest() {
    int x = 5, y = 10;
    assert(x + y == 15); // נכון!
    assert(x + y == 20); // יקרוס בזמן ריצה אם הטענה לא תקפה.
}

int main() {
    addTest(); // ריצת טסט
    return 0;
}
```
סיפוק:
```
Assertion failed: x + y == 20, file example.c, line 6
```

מתקדם:
```C
#include <stdio.h>

void testDivision(double (*divFunc)(double, double)) {
    if(divFunc(10.0, 2.0) == 5.0) {
        printf("Test passed!\n");
    } else {
        printf("Test failed!\n"); 
    }
}

double divide(double a, double b) {
    return a / b;
}

int main() {
    testDivision(divide); // ריצת טסט לפונקציה divide
    return 0;
}
```
סיפוק:
```
Test passed!
```

## Deep Dive: (צלילה עמוקה)
ב-assertions כבר היו בשימוש בשנות ה-70 כחלק משפות תכנות. במהלך השנים, פיתחו מסגרות טסטים מתקדמות כמו JUnit ל-Java ו-CppUnit ל-C++. ב-C, אפשר להשתמש במסגרות כמו cmocka ו-Unity. פרטים נוספים כוללים הבנת הצורך במודולריות ופיצול קוד ליחידות בדיקה.

## See Also: (ראו גם)
- [CUnit - A Unit Testing Framework for C](http://cunit.sourceforge.net/)
- [cmocka – unit testing framework](https://cmocka.org/)
- [Unity - Simple Unit Testing for C](https://www.throwtheswitch.org/unity)
- [Google Test - Google Testing and Mocking Framework](https://github.com/google/googletest)
