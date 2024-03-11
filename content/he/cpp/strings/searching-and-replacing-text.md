---
date: 2024-01-20 17:57:18.949605-07:00
description: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA\
  \ \u05D8\u05E7\u05E1\u05D8 \u05D4\u05D9\u05D0 \u05D0\u05D5\u05E4\u05DF \u05E9\u05D1\
  \u05D5 \u05DE\u05D5\u05E6\u05D0\u05D9\u05DD \u05E8\u05E6\u05E4\u05D9 \u05EA\u05D5\
  \u05D5\u05D9\u05DD \u05D1\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  \ \u05D5\u05DE\u05D7\u05DC\u05D9\u05E4\u05D9\u05DD \u05D0\u05D5\u05EA\u05DD \u05D1\
  \u05E8\u05E6\u05E4\u05D9\u05DD \u05D0\u05D7\u05E8\u05D9\u05DD. \u05EA\u05DB\u05E0\
  \u05D9\u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA\
  \ \u05DB\u05D3\u05D9 \u05DC\u05E2\u05D3\u05DB\u05DF \u05E0\u05EA\u05D5\u05E0\u05D9\
  \u05DD, \u05DC\u05EA\u05E7\u05DF \u05E9\u05D2\u05D9\u05D0\u05D5\u05EA \u05D0\u05D5\
  \ \u05DC\u05DE\u05E0\u05D5\u05E2 \u05E7\u05D5\u05D3\u2026"
lastmod: '2024-03-11T00:14:13.300980-06:00'
model: gpt-4-1106-preview
summary: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA \u05D8\
  \u05E7\u05E1\u05D8 \u05D4\u05D9\u05D0 \u05D0\u05D5\u05E4\u05DF \u05E9\u05D1\u05D5\
  \ \u05DE\u05D5\u05E6\u05D0\u05D9\u05DD \u05E8\u05E6\u05E4\u05D9 \u05EA\u05D5\u05D5\
  \u05D9\u05DD \u05D1\u05EA\u05D5\u05DA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D5\
  \u05DE\u05D7\u05DC\u05D9\u05E4\u05D9\u05DD \u05D0\u05D5\u05EA\u05DD \u05D1\u05E8\
  \u05E6\u05E4\u05D9\u05DD \u05D0\u05D7\u05E8\u05D9\u05DD. \u05EA\u05DB\u05E0\u05D9\
  \u05EA\u05E0\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\
  \u05D3\u05D9 \u05DC\u05E2\u05D3\u05DB\u05DF \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  , \u05DC\u05EA\u05E7\u05DF \u05E9\u05D2\u05D9\u05D0\u05D5\u05EA \u05D0\u05D5 \u05DC\
  \u05DE\u05E0\u05D5\u05E2 \u05E7\u05D5\u05D3\u2026"
title: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA \u05D8\
  \u05E7\u05E1\u05D8"
---

{{< edit_this_page >}}

## מה ולמה?
חיפוש והחלפת טקסט היא אופן שבו מוצאים רצפי תווים בתוך מחרוזת ומחליפים אותם ברצפים אחרים. תכניתנים עושים זאת כדי לעדכן נתונים, לתקן שגיאות או למנוע קוד כפול.

## איך לעשות:
```C++
#include <iostream>
#include <string>
#include <algorithm>

void searchAndReplace(std::string& source, const std::string& search, const std::string& replace) {
    size_t pos = 0;
    while ((pos = source.find(search, pos)) != std::string::npos) {
        source.replace(pos, search.length(), replace);
        pos += replace.length();
    }
}

int main() {
    std::string text = "שלום עולם, שלום חברים!";
    std::string searchText = "שלום";
    std::string replaceText = "להתראות";
    
    searchAndReplace(text, searchText, replaceText);
    std::cout << text << std::endl; // להתראות עולם, להתראות חברים!
    
    return 0;
}
```

## עיון מעמיק:
מקורה של פעולת חיפוש והחלפה מתייחס לעיבוד טקסטים שנעשה עוד בימי מעבדי התמלילים האלקטרוניים. ב-C++, אפשר להשתמש ב`std::string::find` ו-`std::string::replace` לביצוע הפעולות האלה. יש אלטרנטיבות כמו regex (ביטויים רגולריים) המאפשרות חיפוש והחלפה מתוחכמת יותר. לעיתים, נעשית שימוש בספריות חיצוניות לביצוע פעולות אלו באופן יעיל יותר, כמו `boost::algorithm::replace_all`.

## ראה גם:
- [std::string::find](http://www.cplusplus.com/reference/string/string/find/)
- [std::string::replace](http://www.cplusplus.com/reference/string/string/replace/)
- [דוקומנטציה של הספריה Boost](https://www.boost.org/doc/libs/1_75_0/doc/html/string_algo/usage.html)
- [מאמר על עבודה עם ביטויים רגולריים ב-C++](https://www.regular-expressions.info/stdregex.html)
