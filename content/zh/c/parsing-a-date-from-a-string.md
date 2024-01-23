---
title:                "从字符串解析日期"
date:                  2024-01-20T15:34:45.870745-07:00
html_title:           "Arduino: 从字符串解析日期"
simple_title:         "从字符串解析日期"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)

解析日期就是从字符串中提取日期信息。程序员这么做是因为他们需要处理和储存来自文本格式的日期数据。

## How to: (如何操作：)

在C语言中解析日期字符串，通常使用`strptime`函数。下面是一个简单例子，展示了如何使用这个函数。

```c
#include <stdio.h>
#include <time.h>

int main() {
    const char *date_string = "2023-03-15";
    struct tm tm;
    if (strptime(date_string, "%Y-%m-%d", &tm) != NULL) {
        printf("Year: %d, Month: %d, Day: %d\n", tm.tm_year + 1900, tm.tm_mon + 1, tm.tm_mday);
    } else {
        printf("Failed to parse the date.\n");
    }
    return 0;
}
```

运行结果:
```
Year: 2023, Month: 3, Day: 15
```

## Deep Dive (深入探讨)

解析日期由来已久，对早期软件系统至关重要。过去，日期和时间的解析处理多用自制的算法，但这容易出错。随着C标准库的成熟，一些专门的函数出现了，例如`strptime`和`strftime`，它们简化了日期时间的解析和格式化过程。

实际上，在某些系统中，`strptime`未必可用。这时，你可能需要用其他的库，比如`getdate`函数，或者使用第三方库。

当解析日期时，考虑时区和本地化是关键。`struct tm`结构体能够储存这些信息，但解析时需注意。

## See Also (另请参阅)

- C标准库文档: https://en.cppreference.com/w/c/chrono
- `strptime`函数教程: https://www.gnu.org/software/libc/manual/html_node/Low_002dLevel-Time-String-Parsing.html
- 关于C日期和时间处理的深入讨论: https://en.wikipedia.org/wiki/C_date_and_time_functions
