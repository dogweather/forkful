---
title:                "日付を比較する"
date:                  2024-01-20T17:32:27.574596-07:00
model:                 gpt-4-1106-preview
simple_title:         "日付を比較する"

category:             "C"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
日付を比較するって？プログラム中で2つの日付を比べることだよ。なぜ比べるの？予約システムで期限が切れてないか確かめるとか、イベントが過去か未来か判定するときに必要だね。

## How to: (やり方)
```C
#include <stdio.h>
#include <time.h>

int compare_dates(struct tm date1, struct tm date2) {
    // mktime() converts tm structure to time_t
    time_t t1 = mktime(&date1);
    time_t t2 = mktime(&date2);

    if (t1 < t2) return -1; // date1 is earlier
    if (t1 > t2) return 1;  // date1 is later
    return 0;               // dates are equal
}

int main() {
    struct tm date1 = { .tm_year=122, .tm_mon=11, .tm_mday=3 }; // 2022-12-03
    struct tm date2 = { .tm_year=122, .tm_mon=11, .tm_mday=10 }; // 2022-12-10

    int result = compare_dates(date1, date2);
    if (result == -1) printf("date1 is earlier\n");
    else if (result == 1) printf("date1 is later\n");
    else printf("dates are the same\n");

    return 0;
}
```
Sample output:

```
date1 is earlier
```

## Deep Dive (掘り下げ)
Comparing dates is common in calendar apps, booking systems, etc. Before the C standard library had `time.h`, programmers wrote custom functions for date comparison. Two main alternatives for date comparison: `difftime()` and direct `struct tm` member comparison. Remember: direct comparison doesn't account for Daylight Saving Time and other nuances.

## See Also (関連情報)
- C Standard Library Reference: `time.h` - https://en.cppreference.com/w/c/chrono
- Tutorial on Date and Time in C - https://www.tutorialspoint.com/c_standard_library/c_function_difftime.htm
- Understanding `struct tm` and `time_t` - https://www.gnu.org/software/libc/manual/html_node/Broken_002ddown-Time.html
