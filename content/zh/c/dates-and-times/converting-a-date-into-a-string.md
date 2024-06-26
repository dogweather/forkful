---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:54:17.234890-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u4E3A\u6B64\u5E38\u7528\u7684\u51FD\u6570\
  \u662F `<time.h>` \u5E93\u4E2D\u7684 `strftime` \u51FD\u6570\u3002\u5B83\u5141\u8BB8\
  \u4F60\u901A\u8FC7\u6307\u5B9A\u683C\u5F0F\u8BF4\u660E\u7B26\uFF0C\u4EE5\u591A\u79CD\
  \u65B9\u5F0F\u683C\u5F0F\u5316\u65E5\u671F\u548C\u65F6\u95F4\u3002\u4EE5\u4E0B\u662F\
  \u4E00\u4E2A\u5FEB\u901F\u793A\u4F8B\uFF1A."
lastmod: '2024-04-05T22:38:47.472431-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u4E3A\u6B64\u5E38\u7528\u7684\u51FD\u6570\
  \u662F `<time.h>` \u5E93\u4E2D\u7684 `strftime` \u51FD\u6570\u3002\u5B83\u5141\u8BB8\
  \u4F60\u901A\u8FC7\u6307\u5B9A\u683C\u5F0F\u8BF4\u660E\u7B26\uFF0C\u4EE5\u591A\u79CD\
  \u65B9\u5F0F\u683C\u5F0F\u5316\u65E5\u671F\u548C\u65F6\u95F4\u3002\u4EE5\u4E0B\u662F\
  \u4E00\u4E2A\u5FEB\u901F\u793A\u4F8B\uFF1A."
title: "\u5C06\u65E5\u671F\u8F6C\u5316\u4E3A\u5B57\u7B26\u4E32"
weight: 28
---

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
