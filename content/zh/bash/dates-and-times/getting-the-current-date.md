---
title:                "获取当前日期"
aliases: - /zh/bash/getting-the-current-date.md
date:                  2024-02-03T19:08:50.047675-07:00
model:                 gpt-4-0125-preview
simple_title:         "获取当前日期"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？
在 Bash 中获取当前日期涉及使用内置命令以各种格式显示日期和时间。程序员使用这个功能来执行任务，例如给日志打时间戳、安排任务，或者只是作为他们系统信息脚本的一部分，以跟踪何时执行了操作。

## 如何操作：
在 Bash 中，`date` 命令是获取当前日期和时间的主要工具。以下是一些使用它的示例：

1. **以默认格式获取当前日期和时间：**

```bash
date
```

*示例输出：*
```
Wed Apr 5 14:22:04 PDT 2023
```

2. **自定义输出格式：** 你可以使用 `+%` 格式说明符指定输出格式。例如，显示日期为 YYYY-MM-DD 格式：

```bash
date "+%Y-%m-%d"
```

*示例输出：*
```
2023-04-05
```

3. **获取当前 UNIX 时间戳：** UNIX 时间戳是自 Unix 纪元（1970 年 1 月 1 日）以来的秒数。这对于执行基于时间差的计算的脚本很有用。

```bash
date "+%s"
```

*示例输出：*
```
1672877344
```

对于这种基本操作，通常不需要使用流行的第三方库，因为内置的 `date` 命令提供了全面的功能。然而，对于更高级的日期和时间操作，程序员可能会使用其他编程语言或工具，这些工具提供了日期算术和解析的库，例如 Python 的 `datetime` 模块。
