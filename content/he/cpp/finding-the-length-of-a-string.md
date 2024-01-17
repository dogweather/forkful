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

# מה ולמה?
מציאת אורך של מחרוזת היא פעולה שבה מוחזר מספר התווים במחרוזת. פעולה זו חשובה למתכנתים כיוון שהיא מאפשרת לנו לעבוד עם מחרוזות בצורה מדויקת יותר ולהבין את המבנה שלהן.

# איך לעשות:
```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
  string name = "John";
  int length = name.length();
  cout << "The length of the string is: " << length << endl;
  return 0;
}
```
פלט:
```
The length of the string is: 4
```
# צלילה עמוקה:
(1) ראשית, צריך להפעיל את הספרייה הנכונה כדי להשתמש בפעולה length. בשפת C++, ניתן להשתמש בהצהרת "#include <string>". (2) חייבים לשים לב שהפעולה length מחזירה מספר שלם ולא את כל התווים במחרוזת. (3) ניתן להשתמש גם בפעולת size כדי למצוא את אורך המחרוזת.

# ראה גם:
לפרטים נוספים על מציאת אורך של מחרוזת בשפת C++, ניתן לקרוא עוד במדריך הרשמי של C++: https://en.cppreference.com/w/cpp/string/basic_string/length