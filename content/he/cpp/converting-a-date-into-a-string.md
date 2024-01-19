---
title:                "המרת תאריך למחרוזת"
html_title:           "Bash: המרת תאריך למחרוזת"
simple_title:         "המרת תאריך למחרוזת"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## מה זה ולמה?
המרת תאריך למחרוזת ב-C++ היא תהליך של שינוי מבנה תאריך למחרוזת המייצגת אותו. מתכנתים עושים את זה כדי להפוך נתונים של לתאריך לקל לקריאה וכי מחרוזות הן דרך נוחה יותר לשמור ולהעביר מידע.

## איך לעשות:
```C++
#include <iostream>
#include <ctime>

int main() {
    time_t now = time(0);
    char* dt = ctime(&now);

    std::cout << "The local date and time is: " << dt << std::endl;

    return 0;
}
```
פלט לדוגמה יהיה: 
```
The local date and time is: Wed Sep 15 14:45:05 2021
```

## הצצה עמוקה יותר
מאז ומתמיד, התאריך היה אחד המקרים הנפוצים ביותר של מידע שמתכנתים צריכים להנהלה. ב-C++, ישנן שיטות אלטרנטיביות להמרה של תאריך למחרוזת, כולל שימוש במחלקת `std::chrono`, אבל השיטה שהוצגה במדריך הזה היא זו המצויינת תוך כדי שמירה על הפשטות. עשוי להיות שינויים קלים בעת המרת נתוני תאריך למחרוזת, תלוי באזור התאריך המקומי של המערכת.

## ראה גם
יש דרכים רבות אחרות לעשות זאת, ויחד עם כלי מידע קשור. קישורים מומלצים בהמשך:
1. פוסט באתר StackOverflow - "C++ Convert date from string": 
https://stackoverflow.com/questions/10097894/c-convert-date-from-string
2. המדריך C++ למונח "המרת מחרוזת לתאריך", מאת cppreference: 
https://cppreference.com/w/cpp/io/manip/get_time