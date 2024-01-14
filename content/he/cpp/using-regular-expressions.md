---
title:                "C++: שימוש בביטויים רגילים"
simple_title:         "שימוש בביטויים רגילים"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## למה
סכימות רגילות הן כלי כתיבה חזק שיכול לסייע לשפר את הביצועים ולהפוך את התהליך של כתיבת קוד לפשוט יותר. הם מספקים דרך בה ניתן לחפש ולהתאים מחרוזות בצורה מדויקה ומהירה. אם אתם מחפשים דרך טובה יותר לעבוד עם מחרוזות, אז סכימות רגילות הן כלי שאתם צריכים.

## איך לעשות זאת

```C++
#include <regex>
// השתמשו במודול regex להשתמש בסכימות רגילות
 
int main()
{
    std::string text = "דגים דג דגים חמסה דגים גדולים";
    std::regex reg("דגים");
    // יצירת אובייקט של סכימת רגילה עם המחרוזת שאנחנו מחפשים
    std::sregex_iterator it(text.begin(), text.end(), reg);
    // יצירת איטרטור שיכול לאתר את כל המחרוזות שתואמות את הסכימה
    std::sregex_iterator end;
    // מעבר על כל התוצאות והדפסתן
    for (; it != end; it++)
    {
        std::smatch m = *it;
        // *it מכיל את התוצאה התואמת, אנחנו משרשרים את התוצאה לתוצאה חדשה כדי להשתמש בה בהמשך
        std::cout << m.str() << std::endl;
    }
}
```
### פלט:
דגים
דגים
דגים

## צלילה מעמוקת
כדי להשתלט על מלאיות הביטויים התזה והשתלטות בכלי זה בצורה בינונית או מתקדמת יותר, כמו חיסכון של זכרון על ידי השתמשות בגבולות פשוטים, צריך לדעת את כל סוגי הסכימות הרגילות שקיימים ואת התנאים שניתן לשים בתוכם, לדוגמה פקיומני מופע פה.

## ראו גם
- [Documentation for C++ Regular Expressions](https://www.cplusplus.com/reference/regex/)
- [Mastering Regular Expressions - Book by Jeffrey E.F. Friedl](https://www.oreilly.com/library/view/mastering-regular-expressions/9780596528126/)
- [Online Regular Expression Tester](https://