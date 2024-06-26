---
date: 2024-01-26 01:09:35.096306-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1\u05D5\u05D0\
  \u05D5 \u05E0\u05D9\u05E7\u05D7 \u05DE\u05E9\u05D9\u05DE\u05D4 \u05E0\u05E4\u05D5\
  \u05E6\u05D4: \u05D7\u05D9\u05E9\u05D5\u05D1 \u05E9\u05D8\u05D7 \u05E9\u05DC \u05DE\
  \u05E2\u05D2\u05DC. \u05D1\u05DE\u05E7\u05D5\u05DD \u05DC\u05DB\u05EA\u05D5\u05D1\
  \ \u05D0\u05EA \u05D0\u05D5\u05EA\u05D4 \u05D4\u05E0\u05D5\u05E1\u05D7\u05D4 \u05D1\
  \u05DB\u05DC \u05E4\u05E2\u05DD, \u05D0\u05E0\u05D7\u05E0\u05D5 \u05DE\u05E7\u05E4\
  \u05E1\u05DC\u05D9\u05DD \u05D0\u05D5\u05EA\u05D4 \u05DC\u05EA\u05D5\u05DA \u05E4\
  \u05D5\u05E0\u05E7\u05E6\u05D9\u05D4."
lastmod: '2024-03-13T22:44:39.845100-06:00'
model: gpt-4-1106-preview
summary: "\u05D1\u05D5\u05D0\u05D5 \u05E0\u05D9\u05E7\u05D7 \u05DE\u05E9\u05D9\u05DE\
  \u05D4 \u05E0\u05E4\u05D5\u05E6\u05D4."
title: "\u05E1\u05D9\u05D3\u05D5\u05E8 \u05E7\u05D5\u05D3 \u05DC\u05EA\u05D5\u05DA\
  \ \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA"
weight: 18
---

## איך לעשות:
בואו ניקח משימה נפוצה: חישוב שטח של מעגל. במקום לכתוב את אותה הנוסחה בכל פעם, אנחנו מקפסלים אותה לתוך פונקציה.

```C++
#include <iostream>
#define PI 3.14159

double calculateCircleArea(double radius) {
    return PI * radius * radius;
}

int main() {
    double r = 5.0;
    std::cout << "שטח המעגל עם רדיוס " << r << " הוא " << calculateCircleArea(r) << std::endl;
    return 0;
}
```

דוגמה לפלט:
```
שטח המעגל עם רדיוס 5 הוא 78.5397
```

## צלילה לעומק
באופן היסטורי, פרוצדורות ופונקציות היו עמוד השדרה של תכנות מובנה, שזכה לקידום בשנות ה-60 כדי להתמודד עם בעיות של "קוד ספגטי" בשפות תכנות פקודתיות מוקדמות יותר. חלופות כמו OOP (תכנות מונחה אובייקטים) מלכדות יותר על ידי שילוב הפונקציות עם מבני נתונים. ב-C++, יש לך פונקציות רגילות, שיטות מחלקה (כולל שיטות סטטיות), למבדס (lambdas), ופונקציות תבניות, כל אחת מהן מציעה יתרונות שונים. יישום פונקציות מאורגנות היטב מערב לרוב הקפדה על עקרונות כמו DRY ("אל תחזור על עצמך") ו-SRP (עקרון אחריות יחידה), שפירושו שכל פונקציה עושה דבר אחד בלבד ועושה אותו טוב.

## ראה גם
למידע נוסף על פונקציות ב-C++:
- https://en.cppreference.com/w/cpp/language/functions
- https://www.learncpp.com/cpp-tutorial/77-introduction-to-functions/

לעקרונות עיצוב הקשורים בפונקציות:
- https://en.wikipedia.org/wiki/Single-responsibility_principle
- https://en.wikipedia.org/wiki/Don%27t_repeat_yourself

למד על למבדס ושימוש מתקדם בפונקציות:
- https://www.cprogramming.com/c++11/c++11-lambda-closures.html
- https://isocpp.org/wiki/faq/cpp14-language#lambda-captures
