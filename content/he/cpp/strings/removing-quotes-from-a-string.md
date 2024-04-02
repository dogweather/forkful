---
date: 2024-01-26 03:39:19.537576-07:00
description: "\u05D4\u05E1\u05E8\u05EA \u05E6\u05D9\u05D8\u05D5\u05D8\u05D9\u05DD\
  \ \u05DE\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DE\u05E9\u05DE\u05E2\u05D4 \u05DC\
  \u05E7\u05DC\u05E3 \u05D0\u05EA \u05D0\u05D5\u05EA\u05DD \u05EA\u05D5\u05D5\u05D9\
  \ \u05D2\u05E8\u05E9 \u05D0\u05D5 \u05D2\u05E8\u05E9\u05D9\u05D9\u05DD \u05DE\u05E2\
  \u05E6\u05D1\u05E0\u05D9\u05DD \u05E9\u05DE\u05E7\u05D9\u05E4\u05D9\u05DD \u05D0\
  \u05EA \u05D4\u05D8\u05E7\u05E1\u05D8 \u05E9\u05DC\u05E0\u05D5 (' \u05D0\u05D5 \"\
  ). \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05DC\u05E2\u05D9\u05EA\u05D9\
  \u05DD \u05E7\u05E8\u05D5\u05D1\u05D5\u05EA \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\
  \u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05E0\u05E7\u05D5\u05EA \u05E7\u05DC\u05D8\
  , \u05DC\u05D0\u05D7\u05E1\u05DF\u2026"
lastmod: '2024-03-13T22:44:39.816133-06:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05E1\u05E8\u05EA \u05E6\u05D9\u05D8\u05D5\u05D8\u05D9\u05DD \u05DE\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DE\u05E9\u05DE\u05E2\u05D4 \u05DC\u05E7\
  \u05DC\u05E3 \u05D0\u05EA \u05D0\u05D5\u05EA\u05DD \u05EA\u05D5\u05D5\u05D9 \u05D2\
  \u05E8\u05E9 \u05D0\u05D5 \u05D2\u05E8\u05E9\u05D9\u05D9\u05DD \u05DE\u05E2\u05E6\
  \u05D1\u05E0\u05D9\u05DD \u05E9\u05DE\u05E7\u05D9\u05E4\u05D9\u05DD \u05D0\u05EA\
  \ \u05D4\u05D8\u05E7\u05E1\u05D8 \u05E9\u05DC\u05E0\u05D5 (' \u05D0\u05D5 \"). \u05EA\
  \u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD\
  \ \u05E7\u05E8\u05D5\u05D1\u05D5\u05EA \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\
  \u05EA \u05DB\u05D3\u05D9 \u05DC\u05E0\u05E7\u05D5\u05EA \u05E7\u05DC\u05D8, \u05DC\
  \u05D0\u05D7\u05E1\u05DF\u2026"
title: "\u05D4\u05E1\u05E8\u05EA \u05DE\u05E8\u05DB\u05D0\u05D5\u05EA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 9
---

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
