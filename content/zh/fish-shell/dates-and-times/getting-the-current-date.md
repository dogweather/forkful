---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:29.751349-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Fish Shell \u4F7F\u7528\u5916\u90E8\u547D\
  \u4EE4\u5982 `date` \u6765\u83B7\u53D6\u5F53\u524D\u65E5\u671F\uFF0C\u63D0\u4F9B\
  \u4E86\u6839\u636E\u9700\u8981\u683C\u5F0F\u5316\u8F93\u51FA\u7684\u7075\u6D3B\u6027\
  \u3002\u4EE5\u4E0B\u662F\u5982\u4F55\u4F7F\u7528\u5B83\u7684\u65B9\u6CD5\uFF1A."
lastmod: '2024-04-05T22:38:47.415535-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A Fish Shell \u4F7F\u7528\u5916\u90E8\u547D\
  \u4EE4\u5982 `date` \u6765\u83B7\u53D6\u5F53\u524D\u65E5\u671F\uFF0C\u63D0\u4F9B\
  \u4E86\u6839\u636E\u9700\u8981\u683C\u5F0F\u5316\u8F93\u51FA\u7684\u7075\u6D3B\u6027\
  \u3002\u4EE5\u4E0B\u662F\u5982\u4F55\u4F7F\u7528\u5B83\u7684\u65B9\u6CD5\uFF1A."
title: "\u83B7\u53D6\u5F53\u524D\u65E5\u671F"
weight: 29
---

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
