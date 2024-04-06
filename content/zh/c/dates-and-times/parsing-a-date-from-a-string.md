---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:00:12.225827-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A C \u6CA1\u6709\u63D0\u4F9B\u76F4\u63A5\
  \u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F\u7684\u5185\u7F6E\u65B9\u6CD5\uFF0C\
  \u56E0\u6B64\u6211\u4EEC\u7ECF\u5E38\u4F7F\u7528 `<time.h>` \u5E93\u4E2D\u53EF\u7528\
  \u7684 `strptime` \u51FD\u6570\u3002\u8FD9\u4E2A\u51FD\u6570\u4F7F\u6211\u4EEC\u80FD\
  \u591F\u6307\u5B9A\u8F93\u5165\u5B57\u7B26\u4E32\u7684\u9884\u671F\u683C\u5F0F\uFF0C\
  \u5E76\u5C06\u5176\u89E3\u6790\u4E3A `struct tm`\uFF0C\u540E\u8005\u8868\u793A\u5206\
  \u89E3\u6210\u5176\u7EC4\u4EF6\u7684\u65E5\u5386\u65E5\u671F\u548C\u65F6\u95F4\u3002\
  \ \u8FD9\u91CC\u6709\u4E00\u4E2A\u4F7F\u7528 `strptime`\u2026"
lastmod: '2024-04-05T22:38:47.469905-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A C \u6CA1\u6709\u63D0\u4F9B\u76F4\u63A5\u4ECE\
  \u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F\u7684\u5185\u7F6E\u65B9\u6CD5\uFF0C\u56E0\
  \u6B64\u6211\u4EEC\u7ECF\u5E38\u4F7F\u7528 `<time.h>` \u5E93\u4E2D\u53EF\u7528\u7684\
  \ `strptime` \u51FD\u6570\u3002\u8FD9\u4E2A\u51FD\u6570\u4F7F\u6211\u4EEC\u80FD\u591F\
  \u6307\u5B9A\u8F93\u5165\u5B57\u7B26\u4E32\u7684\u9884\u671F\u683C\u5F0F\uFF0C\u5E76\
  \u5C06\u5176\u89E3\u6790\u4E3A `struct tm`\uFF0C\u540E\u8005\u8868\u793A\u5206\u89E3\
  \u6210\u5176\u7EC4\u4EF6\u7684\u65E5\u5386\u65E5\u671F\u548C\u65F6\u95F4\u3002 \u8FD9\
  \u91CC\u6709\u4E00\u4E2A\u4F7F\u7528 `strptime` \u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\
  \u65E5\u671F\u7684\u7B80\u5355\u793A\u4F8B\uFF1A."
title: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u89E3\u6790\u65E5\u671F"
weight: 30
---

## 如何操作：
C 没有提供直接从字符串解析日期的内置方法，因此我们经常使用 `<time.h>` 库中可用的 `strptime` 函数。这个函数使我们能够指定输入字符串的预期格式，并将其解析为 `struct tm`，后者表示分解成其组件的日历日期和时间。

这里有一个使用 `strptime` 从字符串解析日期的简单示例：

```c
#include <time.h>
#include <stdio.h>

int main() {
    const char *dateStr = "2023-04-01";
    struct tm tm;
    char buf[255];

    // 将日期字符串解析成 struct tm
    if (strptime(dateStr, "%Y-%m-%d", &tm) == NULL) {
        printf("解析日期失败。\n");
    } else {
        // 使用 strftime 打印易读格式的日期
        strftime(buf, sizeof(buf), "%A, %B %d, %Y", &tm);
        printf("已解析的日期: %s\n", buf);
    }

    return 0;
}
```

此程序的示例输出如下：

```
已解析的日期: 星期六, 四月 01, 2023
```

处理潜在的错误非常重要，例如 `strptime` 无法匹配模式或遇到意外输入。

## 深入探讨
尽管 `strptime` 函数功能强大，但它不是标准 C 库的一部分，主要在 POSIX 兼容系统（如 Linux 和 UNIX）上找到。这个限制意味着依赖 `strptime` 从字符串解析日期的程序可能无法移植到非 POSIX 系统（如 Windows）而不引入额外的兼容性层或库。

从历史上看，处理 C 中的日期和时间需要大量手动操作和注意，尤其是考虑到不同的地区和时区。C 的现代替代品和扩展，如 C++ 的 `<chrono>` 库和 Howard Hinnant 的 C++ 日期库等第三方库，为日期和时间操作提供了更强大的解决方案，包括解析。这些库通常为更广泛的日期格式、时区和错误处理机制提供更好的支持，使它们更适合需要大量日期和时间操作能力的新项目。

尽管如此，了解如何在 C 中解析字符串中的日期可能是有益的，特别是在需要与那些现代工具不可用或在严格的 C 编程环境中工作的系统兼容的项目上工作或维护时。
