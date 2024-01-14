---
title:                "C++: מחבר מחרוזות"
simple_title:         "מחבר מחרוזות"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## למה

כדי ליצור מחרוזות ארוכות ומורכבות, למשל כאשר מחברים שמות פרטיים ושמות משפחה ליצור שם מלא.

## כיצד לעשות זאת

מחרוזת או מחרוזת הוא סדרה של תווים, המשמשת לאחד מספר פרטים מקשיבים יחד בתכונות.

```C++
#include <iostream>
#include <string>

int main() {
  std::string firstName = "אבי";
  std::string lastName = "כהן";
  std::string fullName = firstName + " " + lastName;
  std::cout << "שלום, אני קוראים לי " << fullName << "."<< std::endl;
  return 0;
}

// Output:
// שלום, אני קוראים לי אבי כהן.
```

כאן, אנו משתמשים באופרטור הפלוס (+) כדי להגדיר מחרוזת חדשה, fullName, המשתמשת במשתנים firstName ו- lastName ובחרימה (" ") ביניהם. בסופו של דבר, אנו מדפיסים את המחרוזת החדשה בסדרת cout.

## מכנס עמוק

בנוסף למשתמשת באופרטור הפלוס (+), ישנן דרכים נוספות למחרוזות מאחדות. למשל:

- שימוש בפונקציות append ו- insert מאפשר להוסיף תווים נוספים למחרוזות קיימות.
- הפונקציה substr מאפשרת לפרק את המחרוזת לחלקים קטנים יותר על ידי הגדרת מיקום התחלה וסופו שלמים.
- ניתן להשתמש בלולאת for להרכיב מחרוזת חדשה מתוך מחרוזות קיימות לפי תנאים מסוימים.

## ראה גם

- [מחרוזת ב- C++](https://he.wikipedia.org/wiki/%D7%9E%D7%97%D7%A8%D7%95%D7%96%D7%AA_(%D7%9B%D7%9C%D7%A9%D7%A0%D7%95%D7%9A))
- [לולאות ב- C++](https://www.tutorialspoint.com/cplusplus/cpp_loop_types.htm)
- [פונקציות מובנות למחרוזת ב- C++](https://www.cplusplus.com/reference/string/string/)