---
date: 2024-01-20 17:47:07.155311-07:00
description: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA \u05D4\u05D9\u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\
  \u05D1\u05D4 \u05D0\u05E0\u05D5 \u05E7\u05D5\u05D1\u05E2\u05D9\u05DD \u05DB\u05DE\
  \u05D4 \u05EA\u05D5\u05D5\u05D9\u05DD \u05D9\u05E9 \u05D1\u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05EA. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\
  \u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D1\u05E6\u05E2 \u05D1\u05D3\
  \u05D9\u05E7\u05D5\u05EA, \u05DC\u05E2\u05D1\u05D5\u05D3 \u05E2\u05DD \u05DC\u05D5\
  \u05DC\u05D0\u05D5\u05EA, \u05D5\u05DC\u05E0\u05D4\u05DC \u05DE\u05D9\u05D3\u05E2\
  \ \u05D1\u05D9\u05E2\u05D9\u05DC\u05D5\u05EA."
lastmod: 2024-02-19 22:04:59.082997
model: gpt-4-1106-preview
summary: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA \u05D4\u05D9\u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05E9\u05D1\
  \u05D4 \u05D0\u05E0\u05D5 \u05E7\u05D5\u05D1\u05E2\u05D9\u05DD \u05DB\u05DE\u05D4\
  \ \u05EA\u05D5\u05D5\u05D9\u05DD \u05D9\u05E9 \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\
  \u05EA. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD\
  \ \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D1\u05E6\u05E2 \u05D1\u05D3\u05D9\
  \u05E7\u05D5\u05EA, \u05DC\u05E2\u05D1\u05D5\u05D3 \u05E2\u05DD \u05DC\u05D5\u05DC\
  \u05D0\u05D5\u05EA, \u05D5\u05DC\u05E0\u05D4\u05DC \u05DE\u05D9\u05D3\u05E2 \u05D1\
  \u05D9\u05E2\u05D9\u05DC\u05D5\u05EA."
title: "\u05DE\u05E6\u05D9\u05D0\u05EA \u05D0\u05D5\u05E8\u05DA \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
מציאת אורך מחרוזת היא פעולה שבה אנו קובעים כמה תווים יש במחרוזת. מתכנתים עושים זאת כדי לבצע בדיקות, לעבוד עם לולאות, ולנהל מידע ביעילות.

## איך לעשות:
```C++
#include <iostream>
#include <string>

int main() {
    std::string myString = "שלום עולם";
    std::cout << "אורך המחרוזת: " << myString.length() << std::endl;
    // או באמצעות size()
    std::cout << "אורך המחרוזת: " << myString.size() << std::endl;
    return 0;
}
```
פלט לדוגמה:
```
אורך המחרוזת: 10
אורך המחרוזת: 10
```

## עיון עמוק
בתחילה, בשפות תכנות מוקדמות כמו C, אורך מחרוזת נמדד באמצעות מעבר על כל התווים עד למציאת תו סיום, '\0'. ב-C++, מחלקת `std::string` מקלה עלינו את העבודה עם הפונקציות `length()` ו-`size()` שמחזירות את אורך המחרוזת. שתי הפונקציות הללו הן בעצם שקולות ומבצעות את אותה הפעולה.

חלופות כוללות את שימוש ב-C-style strings וחישוב אורך באמצעות פונקציה כמו `strlen`, אבל זה פחות מומלץ ב-C++ מכיוון שזה יכול לגרום לשגיאות ריצה וזה לא יעיל כמו שימוש ב-`std::string`.

מבחינת פרטי יישום, `length()` ו-`size()` בדרך כלל מחזירות את הערך של משתנה מסוג `size_t` ששמור בתוך אובייקט המחרוזת, מה שמאפשר מבט ישיר ומהיר לאורך המחרוזת ללא צורך בחישוב.

## ראה גם
- תיעוד המקור למחלקת `std::string` ב-C++: https://en.cppreference.com/w/cpp/string/basic_string
- מדריך למחרוזות ב-C++: https://www.learncpp.com/cpp-tutorial/stdstring/
- איך מחרוזות עובדות ב-C: https://www.tutorialspoint.com/cprogramming/c_strings.htm
