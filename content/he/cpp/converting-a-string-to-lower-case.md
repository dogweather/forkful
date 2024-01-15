---
title:                "המרת מחרוזת לאותיות קטנות"
html_title:           "C++: המרת מחרוזת לאותיות קטנות"
simple_title:         "המרת מחרוזת לאותיות קטנות"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## מדוע

להמרת מחרוזת לאותיות קטנות יש מספר מטרות, בין היתר לצורך השוואת מחרוזות, עיבוד טקסט ומניעת שגיאות בריצה של קוד.

## כיצד לעשות זאת

אחת הדרכים הנפוצות להמרת מחרוזת לאותיות קטנות היא על ידי השתמשות בפונקציה `std::transform` ובאופרטור `tolower`. הנה דוגמה לקוד בשפת C++:

```C++
#include <iostream>
#include <algorithm>
#include <string>

int main() {
  std::string str = "HELLO WORLD";
  // מעביר לכל תו במחרוזת את האופרטור tolower
  std::transform(str.begin(), str.end(), str.begin(), [](unsigned char c){ return std::tolower(c); }); 
  std::cout << str; // מדפיס "hello world" למסך
  return 0;
}
```

## Deep Dive

כאשר נעביר את הפונקציה `tolower` על מחרוזת, היא תעבוד רק על תווים שבין A-Z בזיהוי הסיימול. לכן, שימו לב שהפונקציה לא תעבוד על תווים בלתי תקינים או תווים באותיות קטנות כברירת מחדל.

## See Also

- [מסמך היכולות והאופציות של C++](https://cppreference.com/)
- [פרק ייעודי למחרוזות בקורס של C++ על Codecademy](https://www.codecademy.com/courses/learn-c-plus-plus/lessons/cpp-strings/exercises/review)
- [מאמר על טכניקת "sentence case" בתכנות](https://medium.com/@wobenshain/string-manipulation-tips-for-developers-b09b9ae2b190)