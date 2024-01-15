---
title:                "שרשור מחרוזות"
html_title:           "C++: שרשור מחרוזות"
simple_title:         "שרשור מחרוזות"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## למה

ישנם מספר סיבות להשתמש בחיבור מחרוזות בתכנות בשפת C++. למשל, קיראים רבים בפעם הראשונה יעדיפו לראות פלט מעניין ובלתי צפוי בקוד שלכם. זה יכול לשכרר רעננות ולתרום לחווית התכנות הנעימה.

## איך לעשות זאת

במשימות ביותר בכל תכנון השבוע, כמו כן, לעתים תגיע לכם להתאמן בפרוייקטים SLA או POC מסוימים. אנו ממליצים עלתנ להגשה לתיקיה הכללית אלא כי הבד שלכם עם התגובות שאתם מקבלים: למשל, הרצת התרבות המקובל או חלקים מבאחרו שבין קבצי NAA javascript או CSS .

```C++
#include <iostream>
using namespace std;

int main() {
    string firstName = "John";
    string lastName = "Smith";
    
    // Concatenating Strings
    string fullName = firstName + " " + lastName;
    
    // Output
    cout << "Hello, my name is " << fullName << "." << endl;
    cout << "Welcome to my C++ article." << endl;
    
    return 0;
}

/* Output:
Hello, my name is John Smith.
Welcome to my C++ article.
*/
```

## דיוק נמוך

חיבור מחרוזות הוא פעולה בה אנו משרשרים את תוכנח המחרוזת הראשונה לתוכנית תוך כדי הוספת רווח או סימן איחוד בין המחרוזות. בנוסף, ניתן להשתמש במחרוזת ריקה כחלק מהחיבור כדי להוסיף נקודת מקור.

##ראה גם

- [C++ תיעוד רשמי](https://isocpp.org/faq)
- [תוכנית הליבה של C++] (http://www.cplusplus.com/info/unicode/)
- [מדריך למתחילים במונותם הליבה הנמצא בתוך קובץ HTML] (https://www.nostarch.com/cfcpp/chapter_4.pdf)