---
aliases:
- /zh/c/parsing-a-date-from-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:00:12.225827-07:00
description: "\u5728 C \u4E2D\u89E3\u6790\u5B57\u7B26\u4E32\u4E2D\u7684\u65E5\u671F\
  \u6D89\u53CA\u5C06\u65E5\u671F\u7684\u6587\u672C\u8868\u793A\u8F6C\u6362\u4E3A\u7A0B\
  \u5E8F\u53EF\u4EE5\u66F4\u6709\u6548\u5730\u64CD\u4F5C\u548C\u5206\u6790\u7684\u683C\
  \u5F0F\u3002\u8FD9\u5BF9\u4E8E\u65E5\u671F\u7B97\u672F\u3001\u6BD4\u8F83\u548C\u4E3A\
  \u4E0D\u540C\u5730\u533A\u683C\u5F0F\u5316\u7B49\u4EFB\u52A1\u81F3\u5173\u91CD\u8981\
  \uFF0C\u56E0\u4E3A\u5B83\u5141\u8BB8\u7A0B\u5E8F\u5458\u4EE5\u6807\u51C6\u5316\u7684\
  \u65B9\u5F0F\u5904\u7406\u7528\u6237\u8F93\u5165\u6216\u6570\u636E\u96C6\u6761\u76EE\
  \u3002"
lastmod: 2024-02-18 23:08:59.568562
model: gpt-4-0125-preview
summary: "\u5728 C \u4E2D\u89E3\u6790\u5B57\u7B26\u4E32\u4E2D\u7684\u65E5\u671F\u6D89\
  \u53CA\u5C06\u65E5\u671F\u7684\u6587\u672C\u8868\u793A\u8F6C\u6362\u4E3A\u7A0B\u5E8F\
  \u53EF\u4EE5\u66F4\u6709\u6548\u5730\u64CD\u4F5C\u548C\u5206\u6790\u7684\u683C\u5F0F\
  \u3002\u8FD9\u5BF9\u4E8E\u65E5\u671F\u7B97\u672F\u3001\u6BD4\u8F83\u548C\u4E3A\u4E0D\
  \u540C\u5730\u533A\u683C\u5F0F\u5316\u7B49\u4EFB\u52A1\u81F3\u5173\u91CD\u8981\uFF0C\
  \u56E0\u4E3A\u5B83\u5141\u8BB8\u7A0B\u5E8F\u5458\u4EE5\u6807\u51C6\u5316\u7684\u65B9\
  \u5F0F\u5904\u7406\u7528\u6237\u8F93\u5165\u6216\u6570\u636E\u96C6\u6761\u76EE\u3002"
title: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u89E3\u6790\u65E5\u671F"
---

{{< edit_this_page >}}

## 什么 & 为什么？

在 C 中解析字符串中的日期涉及将日期的文本表示转换为程序可以更有效地操作和分析的格式。这对于日期算术、比较和为不同地区格式化等任务至关重要，因为它允许程序员以标准化的方式处理用户输入或数据集条目。

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
