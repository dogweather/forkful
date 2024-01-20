---
title:                "הדפסת פלט ניפוי שגיאות"
html_title:           "Arduino: הדפסת פלט ניפוי שגיאות"
simple_title:         "הדפסת פלט ניפוי שגיאות"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## מה ולמה?

הדפסת פלט ניפוי הוא תהליך שבו התכנה מדפיסה מידע נוסף העוזר לנו לאבחן תקלות ולבחון את תנודות הקוד. מתכנתים משתמשים בה כשהם מעוניינים למצוא ולתקן שגיאות ולהבין יותר טוב את התנהגות התוכנה.

## איך לעשות:

```C++
#include <iostream>

int main() {
    int variable = 5;
    std::cout << "Debug: The value of variable is " << variable << '\n';
    return 0;
}
```

הפלט של הדוגמה:
```
Debug: The value of variable is 5
```

## בעומק הנושא:

1. היסטוריה: הדפסת פלטים לניפוי באה לידי ביטוי מאז שהתחילה התכנות. היה תמיד צורך להבין את מהלך התכנה. 
2. אלטרנטיבות: Unix/Linux מספקת כלי שנקרא stderr בו אנחנו יכולים לדפיס את הדיבאג.
3. פרטי המימוש: ב-C++ המנגנון פשוט. אנו משתמשים בספריית ה-I/O stream ומשתמשים בפונקציה `std::cout` (console out) להדפסה.

## ראו גם:

1. [std::debug in C++](https://en.cppreference.com/w/cpp/io/cerr)
2. [Debugging with Visual Studio](https://docs.microsoft.com/en-us/visualstudio/debugger/?view=vs-2019)