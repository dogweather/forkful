---
title:                "Отримання поточної дати"
date:                  2024-01-20T15:13:14.746830-07:00
html_title:           "Bash: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Що та Навіщо?
Getting the current date means accessing the system's date and time. Programmers track time to log events, schedule tasks, or display timestamps.

## How to:
Як це зробити:
Here's how to get the current date in C using the `time.h` library:

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t t = time(NULL);
    struct tm tm = *localtime(&t);

    printf("Current date: %02d-%02d-%04d\n", tm.tm_mday, tm.tm_mon + 1, tm.tm_year + 1900);
    return 0;
}
```

Sample output might look like:
```
Current date: 24-04-2023
```

## Deep Dive
Занурення в Деталі
In 1972, `time.h` was standardized in C. Now, `time()` gives seconds since the epoch (00:00:00 UTC, January 1, 1970), and `localtime()` converts it to local time. Alternatives like `gettimeofday()` exist, but it's Unix-specific. `time.h` is widely supported and simple, with `strftime()` to format dates.

## See Also
Додаткова Інформація:
- C Standard Library documentation on `time.h`: https://en.cppreference.com/w/c/chrono
- GNU C Library manual for date and time: https://www.gnu.org/software/libc/manual/html_node/Date-and-Time.html
- Stack Overflow discussions on handling dates and times in C: https://stackoverflow.com/questions/tagged/c+datetime
