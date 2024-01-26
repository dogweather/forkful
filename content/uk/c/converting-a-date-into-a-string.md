---
title:                "Перетворення дати в рядок"
date:                  2024-01-20T17:36:31.000646-07:00
model:                 gpt-4-1106-preview
simple_title:         "Перетворення дати в рядок"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Що і чому?)
Перетворення дати в рядок дозволяє нам зручно зберігати або виводити інформацію про час і дату. Це важливо для логування, інтерфейсів користувача та обміну даними.

## How to: (Як це зробити:)
```C
#include <stdio.h>
#include <time.h>

int main() {
    char dateStr[100];
    time_t now = time(NULL);
    struct tm *ptm = localtime(&now);

    if (ptm == NULL) {
        puts("Error: Could not retrieve the local time.");
        return 1;
    }

    // YYYY-MM-DD format
    strftime(dateStr, sizeof(dateStr), "%Y-%m-%d", ptm);
    printf("The date is: %s\n", dateStr);

    // DD-MM-YYYY format
    strftime(dateStr, sizeof(dateStr), "%d-%m-%Y", ptm);
    printf("Or in another format: %s\n", dateStr);

    return 0;
}
```
Output:
```
The date is: 2023-04-01
Or in another format: 01-04-2023
```

## Deep Dive (Детальний огляд):
Converting a date to a string wasn't always straightforward in C. Before standardized libraries, coders often rolled custom functions. The C Standard Library included `strftime`, allowing predictable date-to-string conversions. With this, developers choose format, respecting locale.

Alternatives? Sure. You could use `sprintf` for simple output but lose `strftime`'s locale awareness and flexibility. For detailed control, dig into `strftime` or time handling libraries; some cater to complex scenarios.

Under the hood, `strftime` takes a `tm` struct, which holds date/time components. It reads this struct and the format string you provide, then outputs a human-readable date/time string. Formats themselves range from complete ISO 8601 strings (`"%FT%TZ"`) to custom arrangements of day, month, year, and even time.

## See Also (Додатково):
- C11 Standard: ISO/IEC 9899:2011 (http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf)
- GNU C Library Reference for `strftime`: https://www.gnu.org/software/libc/manual/html_node/Formatting-Calendar-Time.html#index-strftime
- `time.h` Header: https://en.cppreference.com/w/c/chrono
- Date and Time Utilities in Standard C: https://en.cppreference.com/w/c/chrono
