---
title:                "חיבור מחרוזות"
html_title:           "C++: חיבור מחרוזות"
simple_title:         "חיבור מחרוזות"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## מה & למה?

הדבקת מחרוזות היא פעולה שבה מילים או קטעי מלל משויכים ביניהם כדי ליצור מחרוזת אחת ארוכה יותר. מתכנתים עושים את זה כדי לעבוד ביעילות עם מלל, בין אם זו פלט הדפסה או סדרה של נתונים לעיבוד.

## כיצד לשלב:

הדוגמה הבאה מדגימה כיצד להדביק מחרוזות ב־C++:

```C++
#include <iostream> 
#include <string> 

int main() 
{ 
    std::string str1 = "Hello, "; 
    std::string str2 = "World!"; 

    //Concatenating strings
    std::string str3 = str1 + str2; 

    std::cout << str3; 

    return 0; 
} 
```
פלט מבחן של התוכנית:

```
Hello, World!
```

## צלילה עמוקה:

(1) במסגרת ההיסטורית, בשפות תכנות ראשונות לא היה אפשר להדביק מחרוזות. עם הזמן, התפתחו שפות תכנות מודרניות כמו C++ שמאפשרות הדבקה קלה ויעילה של מחרוזות.

(2) קיימות גם דרכים אחרות להדביק מחרוזות בC++, כמו שימוש בפונקציה `strcat()` או `append()`.

(3) כאשר אתה מבצע הדבקה, המחלקה `std::string` יוצרת מחרוזת חדשה כתוצאה מהיחד של שתי המחרוזות. זו היא פעולה שתלויה בגודל המחרוזות כדי לכמה זמן העומס ידרוש.

## ראה גם:

-   עוד קטע קוד של הדבקה של מחרוזות בעזרת מתודת `append()`: https://www.cplusplus.com/reference/string/string/append/
-   פונקציה `strcat()` בC++: https://www.cplusplus.com/reference/cstring/strcat/