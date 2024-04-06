---
date: 2024-01-20 17:41:45.026729-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05EA\u05D5\u05E6\
  \u05D0\u05EA \u05D4\u05E7\u05D5\u05D3: \u05D4\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  \ \"abc123! XYZ89?\" \u05D4\u05E4\u05DB\u05D4 \u05DC\"abc XYZ\" - \u05D4\u05EA\u05D5\
  \u05D5\u05D9\u05DD \u05E9\u05DC\u05D0 \u05D4\u05EA\u05D0\u05D9\u05DE\u05D5 \u05DC\
  \u05EA\u05D1\u05E0\u05D9\u05EA \u05E0\u05DE\u05D7\u05E7\u05D5."
lastmod: '2024-04-05T21:53:40.885315-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u05DE\u05D7\u05D9\u05E7\u05EA \u05EA\u05D5\u05D5\u05D9\u05DD \u05D4\u05EA\
  \u05D5\u05D0\u05DE\u05D9\u05DD \u05DC\u05EA\u05D1\u05E0\u05D9\u05EA"
weight: 5
---

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
