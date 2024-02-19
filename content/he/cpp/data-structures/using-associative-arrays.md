---
aliases:
- /he/cpp/using-associative-arrays/
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:51.008133-07:00
description: "\u05DE\u05E2\u05E8\u05DB\u05D9\u05DD \u05D0\u05E1\u05D5\u05E6\u05D9\u05D0\
  \u05D8\u05D9\u05D1\u05D9\u05D9\u05DD, \u05D4\u05D9\u05D3\u05D5\u05E2\u05D9\u05DD\
  \ \u05DB-`std::map` \u05D0\u05D5 `std::unordered_map` \u05D1-C++, \u05D2\u05D5\u05E8\
  \u05DE\u05D9\u05DD \u05DC\u05D2\u05E9\u05E8 \u05D1\u05D9\u05DF \u05D0\u05D9\u05E0\
  \u05D3\u05E7\u05E1\u05D9\u05DD \u05E9\u05DC \u05DE\u05E2\u05E8\u05DB\u05D9\u05DD\
  \ \u05DC\u05D1\u05D9\u05DF \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05DE\u05D4\u05E2\
  \u05D5\u05DC\u05DD \u05D4\u05D0\u05DE\u05D9\u05EA\u05D9, \u05D5\u05DE\u05D0\u05E4\
  \u05E9\u05E8\u05D9\u05DD \u05DC\u05DB\u05DD \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9\
  \u2026"
lastmod: 2024-02-18 23:08:53.150786
model: gpt-4-0125-preview
summary: "\u05DE\u05E2\u05E8\u05DB\u05D9\u05DD \u05D0\u05E1\u05D5\u05E6\u05D9\u05D0\
  \u05D8\u05D9\u05D1\u05D9\u05D9\u05DD, \u05D4\u05D9\u05D3\u05D5\u05E2\u05D9\u05DD\
  \ \u05DB-`std::map` \u05D0\u05D5 `std::unordered_map` \u05D1-C++, \u05D2\u05D5\u05E8\
  \u05DE\u05D9\u05DD \u05DC\u05D2\u05E9\u05E8 \u05D1\u05D9\u05DF \u05D0\u05D9\u05E0\
  \u05D3\u05E7\u05E1\u05D9\u05DD \u05E9\u05DC \u05DE\u05E2\u05E8\u05DB\u05D9\u05DD\
  \ \u05DC\u05D1\u05D9\u05DF \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05DE\u05D4\u05E2\
  \u05D5\u05DC\u05DD \u05D4\u05D0\u05DE\u05D9\u05EA\u05D9, \u05D5\u05DE\u05D0\u05E4\
  \u05E9\u05E8\u05D9\u05DD \u05DC\u05DB\u05DD \u05DC\u05D4\u05E9\u05EA\u05DE\u05E9\
  \u2026"
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E2\u05E8\u05DB\u05D9\u05DD\
  \ \u05D0\u05E1\u05D5\u05E6\u05D9\u05D0\u05D8\u05D9\u05D1\u05D9\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?

מערכים אסוציאטיביים, הידועים כ-`std::map` או `std::unordered_map` ב-C++, גורמים לגשר בין אינדקסים של מערכים לבין נתונים מהעולם האמיתי, ומאפשרים לכם להשתמש במפתחות משמעותיים. הם האפשרות לכשאתם זקוקים לחיפושים מהירים, הכנסות ומחיקות באמצעות מפתחות ולא עמדות אינדקס.

## איך לעשות:

ב-C++, מערכים אסוציאטיביים הופכים לחיים עם הכותרות `<map>` ו-`<unordered_map>`. בואו נשבור לדוגמאות כדי לראות את שניהם בפעולה.

### שימוש ב-`std::map`

`std::map` שומר על האלמנטים ממוינים בהתבסס על המפתח. כך אתם מתחילים:

```C++
#include <iostream>
#include <map>
#include <string>

int main() {
    std::map<std::string, int> ageMap;
    
    // הכנסת ערכים
    ageMap["Alice"] = 30;
    ageMap["Bob"] = 25;
    
    // גישה לערכים
    std::cout << "Bob's age: " << ageMap["Bob"] << std::endl;
    
    // איטרציה מעל מפה
    for(const auto &pair : ageMap) {
        std::cout << pair.first << " is " << pair.second << " years old." << std::endl;
    }
    
    return 0;
}
```

### שימוש ב-`std::unordered_map`

כאשר הסדר לא חשוב, אבל הביצועים כן, `std::unordered_map` הוא החבר שלכם, ומציע מורכבות ממוצעת טובה יותר להכנסות, חיפושים ומחיקות.

```C++
#include <iostream>
#include <unordered_map>
#include <string>

int main() {
    std::unordered_map<std::string, double> productPrice;
    
    // הכנסת ערכים
    productPrice["milk"] = 2.99;
    productPrice["bread"] = 1.99;
    
    // גישה לערכים
    std::cout << "Milk price: $" << productPrice["milk"] << std::endl;
    
    // איטרציה מעל unordered_map
    for(const auto &pair : productPrice) {
        std::cout << pair.first << " costs $" << pair.second << std::endl;
    }
    
    return 0;
}
```

## חקירה עמוקה

מערכים אסוציאטיביים ב-C++, במיוחד `std::map` ו-`std::unordered_map`, לא רק עוסקים באחסון אלמנטים. הם מספקים בסיס לניהול נתונים מורכב יותר על ידי האפשרות לבצע פעולות כמו חיפוש, הכנסה והסרה במורכבויות זמן יעילות (לוגריתמי עבור `std::map` ומקרה ממוצע של זמן קבוע עבור `std::unordered_map`). יעילות זו נובעת ממבני הנתונים הבסיסיים: עץ מאוזן עבור `std::map` וטבלת האש ל-`std::unordered_map`.

בהיסטוריה, לפני שהם היו חלק מהספריה התקנית, מתכנתים היו צריכים ליישם את גרסאותיהם העצמיות או להשתמש בספריות צד שלישי, מה שהוביל לחוסר עקביות ואפשרויות לחוסר יעילות. הכללת המפות בספריה התקנית של C++ לא רק הסטנדרטיזה את שימושם אלא גם אופטימלה אותם לביצועים על פני קומפיילרים ופלטפורמות שונות.

למרות ששניהם עוצמתיים, הבחירה בין `std::map` ל-`std::unordered_map` תלויה בפרטים של השימוש הספציפי שלכם. צריכים נתונים ממוינים ולא אכפת לכם מתמורה קלה בביצועים? תבחרו ב-`std::map`. אם אתם אחרי מהירות ולא אכפת לכם מסדר, `std::unordered_map` כנראה האפשרות הטובה יותר עבורכם.

עם זאת, חשוב לציין שכאשר עובדים עם מבני נתונים מורכבים, תמיד ישנם פשרות. במקרים נישה מסוימים, מבני נתונים אחרים או אפילו ספריות של צד שלישי עשויים להציע ביצועים טובים יותר או פונקציונליות המתאימה לצרכים הספציפיים שלכם. תמיד שקלו את האפשרויות שלכם בהתבסס על דרישות הפרויקט שלכם.
