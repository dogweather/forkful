---
title:                "将来または過去の日付を計算する"
date:                  2024-01-20T17:30:47.343852-07:00
model:                 gpt-4-1106-preview
simple_title:         "将来または過去の日付を計算する"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
計算する将来または過去の日付は、特定の日から一定数の日、週、月または年を加算または減算することです。プログラマは、イベント予定、期限の追跡、または時間経過の模倣など、日付を操作する必要があるさまざまな状況でこれを行います。

## How to: (方法)
```c
#include <stdio.h>
#include <time.h>

void addDays(struct tm *date, int daysToAdd) {
    const time_t ONE_DAY = 24 * 60 * 60; // Seconds in one day
    time_t date_seconds = mktime(date) + (daysToAdd * ONE_DAY);
    *date = *localtime(&date_seconds);
}

int main() {
    struct tm date = {0};
    int daysToAdd;
    
    // Set our starting date to January 1st, 2023
    date.tm_year = 123; // Year since 1900
    date.tm_mon = 0;    // January
    date.tm_mday = 1;
    
    printf("Enter number of days to add: ");
    scanf("%d", &daysToAdd);
    
    addDays(&date, daysToAdd);
    
    // Print the future date in YYYY-MM-DD format
    printf("Future date: %d-%02d-%02d\n", date.tm_year + 1900, date.tm_mon + 1, date.tm_mday);
    
    return 0;
}
```

Sample output:
```
Enter number of days to add: 30
Future date: 2023-01-31
```

## Deep Dive (詳細な掘り下げ)
Calculating future or past dates isn't new. In early programming, complex algorithms like Zeller's Congruence were used. Now, the C standard library includes time functions for easier manipulation. We used `mktime` and `localtime` here, but there are alternatives, like `strftime` for formatting.

Time is tricky due to leap years, time zones, and daylight saving. The C library accounts for these, mostly. But if you need time zone specific calculations or deal with historical dates, consider a library like `tz` or `TimeLib`.

Internally, calculations hinge on `time_t`, a type representing seconds since the "epoch" (00:00:00 UTC, January 1, 1970). The function `mktime` converts `struct tm` to `time_t` and we add or subtract seconds to change the date.

## See Also (関連項目)
- The C Standard Library documentation on time.h: http://www.cplusplus.com/reference/ctime/
- GNU C Library Manual on Time: https://www.gnu.org/software/libc/manual/html_node/Time.html
- Time zone library `tz` documentation: https://howardhinnant.github.io/date/tz.html