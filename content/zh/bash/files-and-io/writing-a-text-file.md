---
aliases:
- /zh/bash/writing-a-text-file/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:59.788811-07:00
description: "\u901A\u8FC7 Bash \u7F16\u5199\u6587\u672C\u6587\u4EF6\u53EF\u4EE5\u8BA9\
  \u4F60\u81EA\u52A8\u5316\u6570\u636E\u5B58\u50A8\u3001\u65E5\u5FD7\u8BB0\u5F55\u3001\
  \u914D\u7F6E\u8BBE\u7F6E\u7B49\u3002\u8FD9\u662F shell \u811A\u672C\u7F16\u7A0B\u7684\
  \u4E00\u9879\u57FA\u672C\u6280\u80FD\uFF0C\u4F7F\u7A0B\u5E8F\u5458\u80FD\u591F\u4FDD\
  \u5B58\u547D\u4EE4\u7684\u8F93\u51FA\u3001\u811A\u672C\u6267\u884C\u6216\u7528\u6237\
  \u8F93\u5165\uFF0C\u4EE5\u4FBF\u8FDB\u884C\u62A5\u544A\u3001\u5904\u7406\u6216\u672A\
  \u6765\u6267\u884C\u3002"
lastmod: 2024-02-18 23:08:59.308506
model: gpt-4-0125-preview
summary: "\u901A\u8FC7 Bash \u7F16\u5199\u6587\u672C\u6587\u4EF6\u53EF\u4EE5\u8BA9\
  \u4F60\u81EA\u52A8\u5316\u6570\u636E\u5B58\u50A8\u3001\u65E5\u5FD7\u8BB0\u5F55\u3001\
  \u914D\u7F6E\u8BBE\u7F6E\u7B49\u3002\u8FD9\u662F shell \u811A\u672C\u7F16\u7A0B\u7684\
  \u4E00\u9879\u57FA\u672C\u6280\u80FD\uFF0C\u4F7F\u7A0B\u5E8F\u5458\u80FD\u591F\u4FDD\
  \u5B58\u547D\u4EE4\u7684\u8F93\u51FA\u3001\u811A\u672C\u6267\u884C\u6216\u7528\u6237\
  \u8F93\u5165\uFF0C\u4EE5\u4FBF\u8FDB\u884C\u62A5\u544A\u3001\u5904\u7406\u6216\u672A\
  \u6765\u6267\u884C\u3002"
title: "\u7F16\u5199\u6587\u672C\u6587\u4EF6"
---

{{< edit_this_page >}}

## 什么和为什么？

通过 Bash 编写文本文件可以让你自动化数据存储、日志记录、配置设置等。这是 shell 脚本编程的一项基本技能，使程序员能够保存命令的输出、脚本执行或用户输入，以便进行报告、处理或未来执行。

## 如何操作：

Bash 提供了直接写入文件的简单方法。最常见的使用重定向操作符（`>`、`>>`）和 `tee` 命令。以下是这两种技术的快速介绍。

使用重定向，你可以直接将输出写入文件。操作符 `>` 会写入内容到文件中，如果文件已存在，则替换它；而 `>>` 则在现有文件末尾追加内容，不删除其内容。

```bash
# 使用 > 写入文件
echo "Hello, World!" > myfile.txt

# 使用 >> 向文件追加
echo "This is a new line." >> myfile.txt
```

如果在运行上述命令后检查 `myfile.txt` 的内容，你会发现：

```
Hello, World!
This is a new line.
```

当你希望同时将内容写入文件并在屏幕上（stdout）看到输出时，`tee` 命令非常方便。默认情况下，`tee` 会覆盖文件，但使用 `-a` 标志时，它会向文件追加内容。

```bash
# 使用 tee 写入并显示
echo "Hello, again!" | tee myfile.txt

# 使用 tee -a 追加并显示
echo "Adding another line." | tee -a myfile.txt
```

执行这些命令后，`myfile.txt` 将显示：

```
Hello, again!
Adding another line.
```

虽然 Bash 本身提供了强大的文件操作能力，通过重定向和 `tee` 等命令，但更复杂的操作或更复杂的场景可能需要调用外部工具或脚本语言（例如 Awk、Sed、Python），它们提供了更复杂的文本处理函数。然而，对于大多数简单的文件写入任务，上述方法已经完全足够并被广泛使用。
