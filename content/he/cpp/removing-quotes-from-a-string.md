---
title:                "הסרת מרכאות ממחרוזת"
date:                  2024-01-26T03:39:19.537576-07:00
model:                 gpt-4-0125-preview
simple_title:         "הסרת מרכאות ממחרוזת"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?
הסרת ציטוטים ממחרוזת משמעה לקלף את אותם תווי גרש או גרשיים מעצבנים שמקיפים את הטקסט שלנו (' או "). תכניתנים לעיתים קרובות עושים זאת כדי לנקות קלט, לאחסן טקסט בבסיס נתונים, או להכין מחרוזות לעיבוד נוסף ללא ההפרעה של סימני הציטוט.

## איך לעשות:
הנה דרך פשוטה להיפטר מאותם ציטוטים ב־C++:

```cpp
#include <iostream>
#include <algorithm>

std::string remove_quotes(std::string input) {
    input.erase(std::remove(input.begin(), input.end(), '\"'), input.end());
    input.erase(std::remove(input.begin(), input.end(), '\''), input.end());
    return input;
}

int main() {
    std::string original = R"("Hello, 'World'!")";
    std::string no_quotes = remove_quotes(original);
    std::cout << no_quotes << std::endl;
    return 0;
}
```

הריצו את זה, ותקבלו:

```
Hello, World!
```

הנה! הציטוטים נעלמו.

## צלילה עמוקה
ציטוטים היו מטרד טקסטואלי מאז ראשית המחשוב. בעבר, הייתם רואים תכניתנים שעבדו בקשיים על לולאות דרך כל תו כדי לסנן את אותם ציטוטים. כיום, יש לנו את `std::remove` בספריית התבניות הסטנדרטית (STL) כדי לעשות את העבודה הכבדה.

אלטרנטיבות? בטח! תוכלו להשתמש בביטויים רגולריים עם `std::regex` כדי לזהות ציטוטים, אבל זה קצת כמו להשתמש בפטיש לפצח אגוז - חזק, אך יכול להיות נדבק למשימות פשוטות. לאלה שמעדיפים גרסאות C++ חדישות יותר, יתכן שתרצו לנסות את `std::string_view` לגישות שאינן משנות.

מבחינת יישום, זכרו ש-`std::remove` לא באמת מסיר אלמנטים מהמכולה; הוא מעביר אלמנטים שלא נמחקו קדימה ומחזיר מצביע לאחר הקצה החדש של הטווח. זו הסיבה שאנו זקוקים לשיטת ה־`erase` כדי לחתוך את הזנב הלא רצוי.

## ראה גם
- הפניה ל־C++ `std::remove`: [cppreference.com](https://en.cppreference.com/w/cpp/algorithm/remove)
- עוד על עיבוד `std::string`: [cplusplus.com](http://www.cplusplus.com/reference/string/string/)
- מבוא לביטויים רגולריים ב־C++: [learncpp.com](https://www.learncpp.com/cpp-tutorial/regular-expressions-with-std-regex/)