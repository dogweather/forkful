---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:50.047675-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 Bash \u4E2D\uFF0C`date` \u547D\
  \u4EE4\u662F\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u548C\u65F6\u95F4\u7684\u4E3B\u8981\
  \u5DE5\u5177\u3002\u4EE5\u4E0B\u662F\u4E00\u4E9B\u4F7F\u7528\u5B83\u7684\u793A\u4F8B\
  \uFF1A 1. **\u4EE5\u9ED8\u8BA4\u683C\u5F0F\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u548C\
  \u65F6\u95F4\uFF1A**."
lastmod: '2024-04-05T21:53:48.274941-06:00'
model: gpt-4-0125-preview
summary: "**\u4EE5\u9ED8\u8BA4\u683C\u5F0F\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u548C\
  \u65F6\u95F4\uFF1A**."
title: "\u83B7\u53D6\u5F53\u524D\u65E5\u671F"
weight: 29
---

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
