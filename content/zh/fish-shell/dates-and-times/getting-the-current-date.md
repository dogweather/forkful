---
title:                "获取当前日期"
aliases:
- /zh/fish-shell/getting-the-current-date/
date:                  2024-02-03T19:09:29.751349-07:00
model:                 gpt-4-0125-preview
simple_title:         "获取当前日期"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么和为什么？
在编程中获取当前日期是一项基本任务，它让你能够检索和操作系统的日期和时间数据。在脚本和自动化任务中，这对于生成时间戳、调度任务和创建日志至关重要。

## 如何操作：
Fish Shell 使用外部命令如 `date` 来获取当前日期，提供了根据需要格式化输出的灵活性。以下是如何使用它的方法：

```fish
# 以默认格式显示当前日期
echo (date)

# 输出示例：Wed 25 Oct 2023 15:42:03 BST
```

要自定义日期的格式，你可以使用 `+` 选项，后跟格式指定符：

```fish
# 以YYYY-MM-DD格式显示当前日期
echo (date "+%Y-%m-%d")

# 输出示例：2023-10-25
```

对于更复杂的任务，例如处理时间戳或执行日期运算，Fish Shell 依赖于像 `date` 这样的外部工具，因为它的脚本性质。这是获取当前UNIX时间戳的一个示例：

```fish
# 获取当前UNIX时间戳
echo (date "+%s")

# 输出示例：1666710123
```

并且使用 `date` 来给当前日期加上一天：

```fish
# 给当前日期加上一天
echo (date -d "+1 day" "+%Y-%m-%d")

# 输出示例：2023-10-26
```

注意：示例使用了适用于GNU coreutils的 `date` 命令选项。在其他环境如 macOS 中的选项可能会有所不同，macOS 默认使用 BSD date 命令。请始终参考 `date --help` 或者 man 页面，以获取你环境中的具体细节。
