---
title:                "קבלת התאריך הנוכחי"
html_title:           "C#: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?
ההוצאה של התאריך הנוכחי ב-C++ מאפשרת למתכנתים לראות מתי מה קרה בתוך התוכנית. זה שימושי ביומן, בעת תיעוד שגיאות, ובתהליכים תודעתיים אחרים.

## איך לעשות זאת:
אנחנו משתמשים בספרייה של `<chrono>` ו-`<ctime>` כדי להשיג את התאריך הנוכחי.

```C++
#include <chrono>
#include <ctime>
#include <iostream>

int main() {
    auto now = std::chrono::system_clock::now();
    std::time_t now_c = std::chrono::system_clock::to_time_t(now);
    std::cout << std::ctime(&now_c);
}
```

התוצאה:

```
Wed Mar 3 20:12:49 2021
```

## חפיפה
בעבר נוצרו דרכים אחרות להשיג את התאריך הנוכחי, אך הן היו מורכבות יותר. `<chrono>` הוא הרבה יותר גמיש ומאפשר לנו להשיג נתונים נוספים כמו השעה והדקות. גם `ctime` הוא אלטרנטיבה אפשרית, אך הוא מספק פחות תמיכה בתכנות C++ modern.

שימו לב לתחזוקה לא טובה של `std::ctime` והשפעה שלה על הערך שמחזירה, ['\n'](ניו-ליין), יש גם מספר סביבות שבו דקות-S לא מנותקות תקנית.

## ראה גם
1. [C++ Date & Time - Tutorialspoint](https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm)
2. [std::ctime - cppreference](https://en.cppreference.com/w/cpp/chrono/c/ctime)
3. [std::chrono - cppreference](https://en.cppreference.com/w/cpp/chrono)