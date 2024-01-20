---
title:                "חישוב תאריך בעתיד או בעבר"
html_title:           "C++: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## מה ולמה?
חישוב תאריך בעתיד או בעבר הוא פרוצדורה שבה מותאמת התאריך באמצעות הוספה או החסרה של מספר ימים. מתכנתים נוטים לחשב זאת כדי לנהל צורך זמני, כמו חישוב החזרה או הוספת זמן חסום למערכת.

## איך לבצע:
הנה דוגמא של קוד ב-C++.

```C++
#include <iostream>
#include <ctime>
#include <chrono>

int main() {
    std::time_t now = std::chrono::system_clock::to_time_t(std::chrono::system_clock::now());

    struct tm then = *std::localtime(&now); 

    then.tm_mday += 5;   // add 5 days to the current date. 

    std::mktime(&then);  // normalize the date/time

    char buf[80];
    std::strftime(buf, sizeof(buf), "%Y-%m-%d %H:%M:%S", &then);

    std::cout << "Future Date & Time: " << buf << std::endl;

    return 0;
}
```
יציאת הקוד תהיה כמו למשל: "Future Date & Time: 2023-11-17 15:03:26"

## חקירה מעמיקה:
חישוב תאריך בעתיד או בעבר הוא משימה קיומית בתכנות. אף על פי שמדובר במשימה די פשוטה בקומפסט C++, ישנם שיטות אלטרנטיביות כמו השימוש בספריה `Boost.Date_Time` שממומשת גם בשפות אחרות. `std::mktime` משמש לנרמול התאריך / זמן למידע סטנדרטי.

## ראו גם:
1. [C++ להוסיף ימים לתאריך](https://en.cppreference.com/w/cpp/chrono)
2. [Boost Date_Time](https://www.boost.org/doc/libs/1_75_0/doc/html/date_time.html)
3. [תיעוד הסטנדרט ל- std::mktime](http://www.cplusplus.com/reference/ctime/mktime/)