---
title:                "השוואת שתי תאריכים"
date:                  2024-01-20T17:32:59.244730-07:00
model:                 gpt-4-1106-preview
simple_title:         "השוואת שתי תאריכים"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מה ולמה?
השוואת תאריכים זהו תהליך שבו משווים בין שני נתוני תאריך. מתכנתים עושים את זה כדי לקבוע מי מגיע לפני מי, לחשב פרקי זמן, או לוודא התאמה לתנאים.

## איך לעשות:
שימוש בספריית `<chrono>`:

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    // יצירת שני תאריכים לשוויון
    std::chrono::system_clock::time_point date1 = std::chrono::system_clock::now();
    std::chrono::system_clock::time_point date2 = date1 + std::chrono::hours(24); // תאריך שני, מחר

    // הדפסת התאריכים
    std::time_t date1_time = std::chrono::system_clock::to_time_t(date1);
    std::time_t date2_time = std::chrono::system_clock::to_time_t(date2);
    std::cout << "Date 1: " << std::ctime(&date1_time);
    std::cout << "Date 2: " << std::ctime(&date2_time);

    // השוואת תאריכים
    if (date1 < date2) {
        std::cout << "Date 1 comes before Date 2.\n";
    } else {
        std::cout << "Date 1 comes after or is the same as Date 2.\n";
    }

    return 0;
}
```

פלט לדוגמה:

```
Date 1: Wed Feb 23 21:46:08 2023
Date 2: Thu Feb 24 21:46:08 2023
Date 1 comes before Date 2.
```

## עיון מעמיק
בעבר, השוואת תאריכים נעשתה בעזרת פונקציות של מבנה `tm` או בקוד ידני שיצר השוואות על בסיס מחרוזות ומספרים. ספריית `<chrono>` שהוצגה ב-C++11 והתפתחה בהמשך הופכת את התהליך לגמיש ומדויק יותר. אפשרויות נוספות כוללות שימוש בספריות צד שלישי, כמו Boost.DateTime. לגבי שינויים וייעולים, `<chrono>` נמצא במתמטיקה רבה לשיפורים ותוספות, ולמעשה, בגרסת C++20 יש לנו את `std::chrono::calendar` שמוסיפה ממשקים נוחים גם ליצירה ולהשוואה של תאריכים בлוח השנה.

## ראו גם
- מדריך לספריית `<chrono>`: https://en.cppreference.com/w/cpp/chrono
- ספריית Boost.DateTime: https://www.boost.org/doc/libs/release/doc/html/date_time.html
- ייעולים ב-C++20 לקלנדרים וזמנים: https://en.cppreference.com/w/cpp/chrono/calendar
