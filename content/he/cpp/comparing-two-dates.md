---
title:                "השוואה בין שני תאריכים"
html_title:           "Arduino: השוואה בין שני תאריכים"
simple_title:         "השוואה בין שני תאריכים"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מה ולמה?

השוואת שני תאריכים היא פעולה שבה אנו מחליטים איזה מבין שני התאריכים קורה קודם. מתכנתים עשויים לעבוד על פרויקטים הדורשים סידור של אירועים לפי התאריך שבו הם קרו, לעיתים אף על פי השעה, הדקה או השנייה.

## איך?

הנה דוגמת קוד המשווה בין שני תאריכים באמצעות מחלקת `std::chrono`:

```C++
#include <chrono>
#include <iostream>

int main() {
  using namespace std::chrono;

  // Creating two different system_clock::time_point objects
  system_clock::time_point time_point1 = system_clock::now();
  system_clock::time_point time_point2 = system_clock::now() + hours(24);

  // Comparing the two time_points
  if (time_point2 > time_point1)
    std::cout << "time_point2 is later than time_point1.\n";
  else
    std::cout << "time_point1 is later than or equal to time_point2.\n";

  return 0;
}
```

פלט דוגמה:

```
time_point2 is later than time_point1.
```

## צלילה עמוקה

אם אתה חדש לתכנות ב-C++, תהיה מופתע לגלות שתאריכים מנוהלים באופן שונה לחלוטין מאשר מספרים רגילים. המטמונים של מניהול הזמן בצורה זו נובעים מהיסטוריה של השפה ומתחומי שימוש מגוונים שבהם היא מכניסה.

המנייה של התאריחם שונה ממנייה של סוגים אחרים. נכנסים בניגוד של מניה התאריכים בתוך היסטוריה של איך השפה מתפתחת, אפשרויות חלופות שתוך איזה סביבות היה ניתן להשתמש, ופרטי המימוש של התאריכים בידי מימוש התוכנה עצמה.

## ראו גם

בלינקים המצורפות תמצאו מידע נוסף לתכנות תאריכים ב-C++:

1. [C++ Date & Time](https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm)
2. [Chrono Library](https://en.cppreference.com/w/cpp/chrono)