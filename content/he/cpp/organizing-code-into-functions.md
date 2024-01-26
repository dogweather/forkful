---
title:                "סידור קוד לתוך פונקציות"
date:                  2024-01-26T01:09:35.096306-07:00
model:                 gpt-4-1106-preview
simple_title:         "סידור קוד לתוך פונקציות"
programming_language: "C++"
category:             "C++"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## מה ולמה?
חלוקת הקוד לפונקציות משמעה חיתוך הקוד לקטעים קטנים יותר וניתנים לשימוש חוזר. אנחנו עושים את זה כדי להימנע מחזרה, להפוך את הקוד שלנו לקריא יותר, ולפשט את הדיבאגינג ובדיקת התוכנה. פונקציות מאורגנות היטב יכולות להיות כמו קופסה של כלים מסודרים ומתוייגים היטב, מוכנים לשימוש ושיתוף.

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