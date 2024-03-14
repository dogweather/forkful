---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:50.047675-07:00
description: "\u5728 Bash \u4E2D\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u6D89\u53CA\u4F7F\
  \u7528\u5185\u7F6E\u547D\u4EE4\u4EE5\u5404\u79CD\u683C\u5F0F\u663E\u793A\u65E5\u671F\
  \u548C\u65F6\u95F4\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\u8FD9\u4E2A\u529F\u80FD\u6765\
  \u6267\u884C\u4EFB\u52A1\uFF0C\u4F8B\u5982\u7ED9\u65E5\u5FD7\u6253\u65F6\u95F4\u6233\
  \u3001\u5B89\u6392\u4EFB\u52A1\uFF0C\u6216\u8005\u53EA\u662F\u4F5C\u4E3A\u4ED6\u4EEC\
  \u7CFB\u7EDF\u4FE1\u606F\u811A\u672C\u7684\u4E00\u90E8\u5206\uFF0C\u4EE5\u8DDF\u8E2A\
  \u4F55\u65F6\u6267\u884C\u4E86\u64CD\u4F5C\u3002"
lastmod: '2024-03-13T22:44:47.974274-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Bash \u4E2D\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u6D89\u53CA\u4F7F\
  \u7528\u5185\u7F6E\u547D\u4EE4\u4EE5\u5404\u79CD\u683C\u5F0F\u663E\u793A\u65E5\u671F\
  \u548C\u65F6\u95F4\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\u8FD9\u4E2A\u529F\u80FD\u6765\
  \u6267\u884C\u4EFB\u52A1\uFF0C\u4F8B\u5982\u7ED9\u65E5\u5FD7\u6253\u65F6\u95F4\u6233\
  \u3001\u5B89\u6392\u4EFB\u52A1\uFF0C\u6216\u8005\u53EA\u662F\u4F5C\u4E3A\u4ED6\u4EEC\
  \u7CFB\u7EDF\u4FE1\u606F\u811A\u672C\u7684\u4E00\u90E8\u5206\uFF0C\u4EE5\u8DDF\u8E2A\
  \u4F55\u65F6\u6267\u884C\u4E86\u64CD\u4F5C\u3002"
title: "\u83B7\u53D6\u5F53\u524D\u65E5\u671F"
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
