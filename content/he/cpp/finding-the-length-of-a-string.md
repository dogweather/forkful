---
title:                "מציאת אורך של מחרוזת"
html_title:           "C++: מציאת אורך של מחרוזת"
simple_title:         "מציאת אורך של מחרוזת"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## למה:
ניתן למצוא את אורך של מחרוזת (string) בכדי לאפשר עיבוד וטיפול נוח יותר של נתונים מסוג זה.

## איך לעשות זאת:
אפשר למצוא את אורך המחרוזת בשפת C++ באמצעות הפונקציה `length()` או `size()` שקיימות בספריית הסטנדרטית (Standard Library). ניתן להגדיר מחרוזת עם או בלי מקף סיום ולבדוק את האורך שלה באמצעות הפונקציות הנ"ל. כאן ניתן לראות דוגמא בקוד:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    // מחרוזת עם מקף סיום
    string s1 = "Hello world!";
    
    // מחרוזת בלי מקף סיום
    string s2 = "Hello world";
    
    // הדפסת אורך המחרוזות
    cout << "Length of s1: " << s1.length() << endl;
    cout << "Length of s2: " << s2.size() << endl;
    
    return 0;
}
```

פלט:

```
Length of s1: 12
Length of s2: 11
```

## Deep Dive:
כאשר מחשבים את אורך המחרוזת, התוכנית עוברת על כל התווים במחרוזת עד למציאת תו מיוחד המציין את הסיום של המחרוזת. בדרך כלל זהו התו ASCII המייצג את התו ״נקודת סיום חזקה״ (NULL), שהוא תו יחיד בסוגו ומדגיש את גבול המחרוזת. בכפילות שלמעלה, חישוב אורך המחרוזת לא תמיד יעבוד באופן מיטבי, ויכול להיות קשה יותר להבין מדוע במקרים מסוימים יש צורך להשתמש בפונקציות אחרות כמו `strlen()` בספריית הסטנדרטית של שפת C.

## ראה גם:
- [ספריית הסטנדרטית של C++](https://en.cppreference.com/w/)
- [אורך מחרוזת בשפת C](https://www.geeksforgeeks.org/strlen-function-in-cpp/)
- [הסבר על תווי ASCII](https://he.wikipedia.org/wiki/ASCII)