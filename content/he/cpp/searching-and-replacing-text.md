---
title:                "חיפוש והחלפת טקסט"
date:                  2024-01-20T17:57:18.949605-07:00
model:                 gpt-4-1106-preview
simple_title:         "חיפוש והחלפת טקסט"

category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/searching-and-replacing-text.md"
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
