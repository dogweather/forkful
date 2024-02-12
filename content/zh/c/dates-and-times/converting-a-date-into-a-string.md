---
title:                "将日期转化为字符串"
aliases:
- /zh/c/converting-a-date-into-a-string/
date:                  2024-02-03T17:54:17.234890-07:00
model:                 gpt-4-0125-preview
simple_title:         "将日期转化为字符串"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/converting-a-date-into-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

# 什么与为什么？

在 C 语言中，将日期转换成字符串涉及到将日期结构或时间戳转化成人类可读的格式。程序员通常执行此任务是为了在日志、用户界面中显示日期，或当需要将日期存储在文本格式（如 JSON 或 CSV）时。

## 如何操作：

为此常用的函数是 `<time.h>` 库中的 `strftime` 函数。它允许你通过指定格式说明符，以多种方式格式化日期和时间。以下是一个快速示例：

```c
#include <stdio.h>
#include <time.h>

int main() {
    char dateStr[100];
    time_t now = time(NULL);
    struct tm *ptm = localtime(&now);

    // 将日期和时间转换为字符串（例如："Wed Jun 30 21:49:08 2021"）
    strftime(dateStr, sizeof(dateStr), "%a %b %d %H:%M:%S %Y", ptm);
    
    printf("当前日期和时间：%s\n", dateStr);
    return 0;
}
```

示例输出可能看起来像这样：

```
当前日期和时间：Wed Jun 30 21:49:08 2021
```

你可以通过更改传递给 `strftime` 的格式说明符来自定义格式。例如，要以 `YYYY-MM-DD` 的格式获取日期，你可以使用 `"%Y-%m-%d"`。

## 深入了解

`strftime` 函数和 `<time.h>` 库是 C 标准库的一部分，可追溯至最初的 ANSI C 标准（C89/C90）。虽然这种方法直接且在许多平台上得到支持，与提供更直观的日期和时间库的现代编程语言相比，这种方式可能看起来低级且繁琐。

应当注意的是，尽管 C 标准库的时间函数得到广泛支持且相对简单易用，它们缺少在新语言的库或第三方 C 库（如国际组件 Unicode（ICU））中找到的一些更复杂的时区操作和国际化功能。

然而，`strftime` 函数的自定义能力和广泛的平台支持使其成为 C 中日期字符串转换的可靠且有用的工具。来自具有更高级日期时间库语言的程序员可能需要适应其低级性质，但会发现它对于格式化日期和时间以适应各种应用场景来说非常强大且灵活。
