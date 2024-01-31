---
title:                "Обчислення дати у майбутньому або минулому"
date:                  2024-01-20T17:31:00.155073-07:00
model:                 gpt-4-1106-preview
simple_title:         "Обчислення дати у майбутньому або минулому"

category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why? / Що і Чому?
Обчислення дат у майбутньому або минулому — це процес знаходження дат через певний проміжок часу. Програмісти роблять це для планування подій, обліку, ремайндерів, тощо.

## How to: / Як це зробити:
```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t now;
    struct tm newdate;
    double seconds;

    time(&now);  // Поточний час
    newdate = *localtime(&now);

    // Додати 10 днів до поточної дати
    newdate.tm_mday += 10;
    mktime(&newdate);
    
    char buffer[80];
    strftime(buffer, 80, "Дата через 10 днів: %d-%m-%Y", &newdate);
    printf("%s\n", buffer);

    // Відняти 30 днів від поточної дати
    newdate = *localtime(&now);  // Скидання до поточної дати
    newdate.tm_mday -= 30;
    mktime(&newdate);

    strftime(buffer, 80, "Дата 30 днів назад: %d-%m-%Y", &newdate);
    printf("%s\n", buffer);
    
    return 0;
}
```

Sample output / Приклад результату:
```
Дата через 10 днів: 23-04-2023
Дата 30 днів назад: 14-03-2023
```

## Deep Dive / Поглиблений Розділ:
In the past, handling dates in C was cumbersome. The C standard library, though, simplified this with the `time.h` library providing time-related functions and structures like `time_t` and `struct tm`.

Before this, coders often dealt with dates manually, sometimes resulting in bugs around corner cases like leap years. Using the `time.h` library, however, ensures correct handling of these details.

Alternatives? You've got libraries like `date.h` or systems like Joda-Time in Java that offer richer functionality but aren't part of standard C.

As for implementation details, note `mktime()` normalizes the `struct tm` instance when you've added or subtracted days, accounting for month’s end, year’s end, leap years, etc. Also, be aware of `localtime()` and `gmtime()` differences; the former converts `time_t` to local time, the latter to UTC.

## See Also / Дивись Також:
- C Standard Library reference: https://en.cppreference.com/w/c/chrono
- GNU C Library – Time Functions: https://www.gnu.org/software/libc/manual/html_node/Time-Functions.html
- “Date and Time in C” – a comprehensive guide: https://www.cprogramming.com/tutorial/date_time.html
- For more advanced date-time manipulations outside the C standard library, check out Howard Hinnant's date library: https://github.com/HowardHinnant/date

Remember, working with dates can get complex, especially with different locales and timezones. For applications requiring such details, consider specialized libraries or APIs.
