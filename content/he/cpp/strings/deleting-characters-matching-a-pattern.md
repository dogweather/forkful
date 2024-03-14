---
date: 2024-01-20 17:41:45.026729-07:00
description: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05E9\
  \u05EA\u05D5\u05D0\u05DE\u05D9\u05DD \u05EA\u05D1\u05E0\u05D9\u05EA \u05D4\u05D9\
  \u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05D1\u05D4 \u05D0\u05E0\u05D7\u05E0\
  \u05D5 \u05DE\u05E1\u05D9\u05E8\u05D9\u05DD \u05EA\u05D5\u05D5\u05D9\u05DD \u05DE\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D1\u05D4\u05EA\u05D0\u05DD \u05DC\u05EA\
  \u05D1\u05E0\u05D9\u05EA \u05DE\u05E1\u05D5\u05D9\u05DE\u05EA. \u05EA\u05DB\u05E0\
  \u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\
  \u05D9 \u05DC\u05E0\u05E7\u05D5\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05DE\
  \u05E8\u05E2\u05E9, \u05DC\u05D0\u05DE\u05EA \u05E7\u05DC\u05D8 \u05DE\u05E9\u05EA\
  \u05DE\u05E9, \u05D0\u05D5 \u05DC\u05D4\u05DB\u05D9\u05DF\u2026"
lastmod: '2024-03-13T22:44:39.809975-06:00'
model: gpt-4-1106-preview
summary: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05E9\u05EA\
  \u05D5\u05D0\u05DE\u05D9\u05DD \u05EA\u05D1\u05E0\u05D9\u05EA \u05D4\u05D9\u05D0\
  \ \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05D1\u05D4 \u05D0\u05E0\u05D7\u05E0\u05D5\
  \ \u05DE\u05E1\u05D9\u05E8\u05D9\u05DD \u05EA\u05D5\u05D5\u05D9\u05DD \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05D1\u05D4\u05EA\u05D0\u05DD \u05DC\u05EA\u05D1\
  \u05E0\u05D9\u05EA \u05DE\u05E1\u05D5\u05D9\u05DE\u05EA. \u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9\
  \ \u05DC\u05E0\u05E7\u05D5\u05EA \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05DE\u05E8\
  \u05E2\u05E9, \u05DC\u05D0\u05DE\u05EA \u05E7\u05DC\u05D8 \u05DE\u05E9\u05EA\u05DE\
  \u05E9, \u05D0\u05D5 \u05DC\u05D4\u05DB\u05D9\u05DF\u2026"
title: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05D4\u05EA\
  \u05D5\u05D0\u05DE\u05D9\u05DD \u05DC\u05EA\u05D1\u05E0\u05D9\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
מחיקת תווים שתואמים תבנית היא פעולה שבה אנחנו מסירים תווים ממחרוזת בהתאם לתבנית מסוימת. תכנתים עושים זאת כדי לנקות נתונים מרעש, לאמת קלט משתמש, או להכין טקסטים לעיבוד נוסף.

## איך לעשות:
```C++
#include <iostream>
#include <regex>
#include <string>

int main() {
    std::string text = "abc123! XYZ89?";
    std::regex pattern("[^a-zA-Z ]"); // הגדרת תבנית לכל דבר שאינו אות אנגלית גדולה או קטנה או רווח
    std::string cleanText = std::regex_replace(text, pattern, "");

    std::cout << cleanText << std::endl; // פלט: abc XYZ

    return 0;
}
```
תוצאת הקוד: המחרוזת "abc123! XYZ89?" הפכה ל"abc XYZ" - התווים שלא התאימו לתבנית נמחקו.

## הצצה לעמוק:
המינוח "הבעות רגולריות" (Regular Expressions) מתייחס לשפת תיאור תבניות ששמה לתכנתים כלי חזק לסינון טקסטים מידע מורכב. השפה נוצרה בשנות ה-50 על ידי סטיבן קליני והשתכללה במרוצת השנים. חלופות כוללות מניפולציית מחרוזות בסיסית יותר, כגון שימוש בפונקציות `find` ו-`erase`, אבל הבעות רגולריות מספקות גמישות רבה ויעילות בפעולות מורכבות יותר. עם זאת, שימוש לא זהיר בהבעות רגולריות עלול להוביל לביצועים גרועים או קוד מבלבל.

## ראה גם:
- [std::regex documentation](http://www.cplusplus.com/reference/regex/)
- [Regex tutorial](https://www.regular-expressions.info/)
- [C++ reference for string manipulation](https://en.cppreference.com/w/cpp/string/basic_string)
