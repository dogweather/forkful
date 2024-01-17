---
title:                "חיפוש והחלפת טקסט"
html_title:           "C++: חיפוש והחלפת טקסט"
simple_title:         "חיפוש והחלפת טקסט"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## מה ולמה?

חיפוש והחלפת טקסט היא פעולה נפוצה שבוצעת על קוד מחשב בצורה שיקולית. הרעיון הוא למצוא מחרוזות ספציפיות בקוד ולהחליף אותן במחרוזות אחרות. מתכנתים מבצעים פעולות כאלה רבות לצורך שימושיות והתאמת קוד לצרכי המטרה הסופית.

## איך לבצע?

שימוש בפונקציות הקיימות של C++ כמו `find` ו-`replace` יכול לעזור בביצוע פעולות חיפוש והחלפה בקוד. לדוגמה:

```C++
string myString = "Hello World!";
string searchString = "Hello";
string replaceString = "Hi";

// מציאת המחרוזת "Hello" והחלפתה ב-"Hi"
size_t found = myString.find(searchString);
if (found != string::npos) {
  myString.replace(found, searchString.length(), replaceString);
}

cout << myString << endl;
```

פלט: Hi World!

## קדימה לעומק

- חיפוש והחלפת טקסט נהיה אדלט בתחומי התכנות כבר מאומה וכנראה יישאר ככה עד לנצח. טכנולוגיות חדשות כגון תיבת דו-קבוצתית (REGEX) מציעות פונקציונליות רבה יותר ביחס לחיפוש והחלפה של טקסט.
- בנוסף לפונקציות שיש ל C++ בתור שפת תכנות ראשית, ישנן גם ספריות חיצוניות המתמחות בחיפוש והחלפת טקסט וכוללות אפשרויות מתקדמות יותר לפעולה זו.
- על מנת למקסם את היעילות של חיפוש והחלפת טקסט, יש להתאים את האלגוריתם לצרכי היישום ולקחת בחשבון גם את המורכבות של המחרוזות.

## ראו גם

- [מדריך פשוט לחיפוש והחלפת טקסט בלולאות בספריית שפות C++](https://www.geeksforgeeks.org/cpp-program-to-search-and-replace-text-in-a-file/)
- [שימוש בפונקציות `find` ו-`replace` בתכנות מונוספי ב-C++](https://www.algosome.com/articles/monospace-find-replace.html)
- [מדריך להשתמש בתיבת דו-קבוצתית (REGEX) לחיפוש והחלפת טקסט ב-C++](https://www.educative.io/m/regular-expressions-in-cpp)
- [הספרייה הפופולרית Boost המציעה פתרונות יעילים לחיפוש והחלפת טקסט בסביבת C++](https://www.boost.org/doc/libs/1_75_0/libs/regex/doc/html/boost_regex/introduction.html)