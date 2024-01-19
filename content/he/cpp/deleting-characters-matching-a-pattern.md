---
title:                "מחיקת תווים התואמים לתבנית"
html_title:           "Elixir: מחיקת תווים התואמים לתבנית"
simple_title:         "מחיקת תווים התואמים לתבנית"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## מה ולמה?
מחיקת תווים שמתאימים לדפוס היא פעולה בה אנו מסירים תווים מסוימים ממחרוזת. מתכנתים בוחרים לבצע את זה כדי לנקות את הנתונים, לשפר את הביצועים או להתמקד בתווים שהם רלוונטיים.
  
## איך עושים את זה?
קוד C++ להקתדים:
```C++
#include <string>
#include <algorithm>

std::string remove_chars(std::string str, std::string chars_to_remove) {
    for (char c : chars_to_remove) {
        str.erase(std::remove(str.begin(), str.end(), c), str.end());
    }
    return str;
}

// דוגמה לשימוש:
std::cout << remove_chars("Hello, World!", "loW") << std::endl; // Output: "He, rld!"
```
פלט מהדוגמה יהיה "He, rld!", כי התווים 'l', 'o' ו- 'W' הוסרו מהמחרוזת.

## הצצה עמוקה
א) במסגרת התולדה של תכנות, הצורך במחיקת תווים מסוימים בא לידי ביטוי במגוון שפות תכנות, בניהן גם C++.
ב) חלופות לפונקציה שנבחרה יכולה להכלול שימוש במערך תווים במקום `std::string` או שימוש בשיטות אחרות למחיקת התווים.
ג) הפונקציה שהוזנה משנה את המחרוזת המקורית. היא מנקה את המחרוזת מהתו 'c' ולאחר מכן חוזרת ומוחקת את כל המקומות ש'c' הוסר מהם ושמאחריהם נפלו תווים המורה על סוף המחרוזת.

## ראה גם
1. [Cplusplus.com - std::remove](http://cplusplus.com/reference/algorithm/remove/)
2. [Cplusplus.com - std::string::erase](http://cplusplus.com/reference/string/string/erase/)
3. [Cplusplus.com - std::string::find](http://cplusplus.com/reference/string/string/find/)