---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:29.751349-07:00
description: "\u5728\u7F16\u7A0B\u4E2D\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u662F\u4E00\
  \u9879\u57FA\u672C\u4EFB\u52A1\uFF0C\u5B83\u8BA9\u4F60\u80FD\u591F\u68C0\u7D22\u548C\
  \u64CD\u4F5C\u7CFB\u7EDF\u7684\u65E5\u671F\u548C\u65F6\u95F4\u6570\u636E\u3002\u5728\
  \u811A\u672C\u548C\u81EA\u52A8\u5316\u4EFB\u52A1\u4E2D\uFF0C\u8FD9\u5BF9\u4E8E\u751F\
  \u6210\u65F6\u95F4\u6233\u3001\u8C03\u5EA6\u4EFB\u52A1\u548C\u521B\u5EFA\u65E5\u5FD7\
  \u81F3\u5173\u91CD\u8981\u3002"
lastmod: '2024-03-13T22:44:48.277623-06:00'
model: gpt-4-0125-preview
summary: "\u5728\u7F16\u7A0B\u4E2D\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u662F\u4E00\
  \u9879\u57FA\u672C\u4EFB\u52A1\uFF0C\u5B83\u8BA9\u4F60\u80FD\u591F\u68C0\u7D22\u548C\
  \u64CD\u4F5C\u7CFB\u7EDF\u7684\u65E5\u671F\u548C\u65F6\u95F4\u6570\u636E\u3002\u5728\
  \u811A\u672C\u548C\u81EA\u52A8\u5316\u4EFB\u52A1\u4E2D\uFF0C\u8FD9\u5BF9\u4E8E\u751F\
  \u6210\u65F6\u95F4\u6233\u3001\u8C03\u5EA6\u4EFB\u52A1\u548C\u521B\u5EFA\u65E5\u5FD7\
  \u81F3\u5173\u91CD\u8981\u3002."
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
