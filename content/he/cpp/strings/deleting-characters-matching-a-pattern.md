---
title:                "מחיקת תווים התואמים לתבנית"
aliases:
- he/cpp/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:41:45.026729-07:00
model:                 gpt-4-1106-preview
simple_title:         "מחיקת תווים התואמים לתבנית"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/deleting-characters-matching-a-pattern.md"
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
