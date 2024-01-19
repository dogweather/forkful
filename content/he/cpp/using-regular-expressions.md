---
title:                "שימוש בביטויים רגילים"
html_title:           "Fish Shell: שימוש בביטויים רגילים"
simple_title:         "שימוש בביטויים רגילים"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## מה ולמה?

סדרי ביטויים רגולריים משמשים למציאת מחרוזות, אימות התאמה, והחלפה במחרוזת. הם נמשכים בשפות תכנות כי הם מאפשרים קריאה ותיקונים מהירים של מידע מטקסט.

## כיצד להשתמש:

להלן דוגמאות לקוד ותוצאות מדגמים ב C++:

```C++
#include <regex>
#include <string>

int main(){
   std::string s = "hello world in 2021";
   std::regex e ("\\b\\w{5}\\b");

   bool match = std::regex_search(s, e);
   printf("match: %d", match);

   return 0;
}
```
זוהי תוכנית בסיסית שמחפשת מחרוזת שמכילה מילים באורך של חמישה תווים. התוצאה תהיה "1" (המשמע היא נמצאה תאמתן) או "0" (לא נמצאה).

## צלילה עמוקה:

סדרי ביטויים רגולריים הוצאו לראשונה לפועל באמצעות שפת Unix ED ב-1970. הם התפתחו למורכבות מעטה מכך, אבל הרעיון הבסיסי נשאר אותו הדבר. לאחרונה, אפשרויות אחרות כמו שפת התכנות Python או ספרייה של Java 8 streams התווספו להם. ההבדל המרכזי בפרטי המימוש הוא שהגרסה של C++ משתמשת בהרבה יותר ממונה מאשר Python או Java.

## ראו גם:

1. [סדרי ביטויים רגולריים ב- C++](https://www.cplusplus.com/reference/regex/)
2. [תיעוד ה- C++ STL regex](http://www.cplusplus.com/reference/regex/)
3. [ניתוח סדרי ביטויים רגולריים](https://en.wikipedia.org/wiki/Regular_expression).