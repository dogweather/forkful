---
date: 2024-01-26 04:11:33.875138-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 Bash \u4E2D\uFF0C\u4F60\u7684\u7EC8\
  \u7AEF\u672C\u8D28\u4E0A\u662F\u4E00\u4E2A REPL\u3002\u4F60\u8F93\u5165\u547D\u4EE4\
  \uFF1B\u5B83\u8BFB\u53D6\u5B83\uFF0C\u6C42\u503C\uFF0C\u6253\u5370\u7ED3\u679C\uFF0C\
  \u5E76\u5FAA\u73AF\u56DE\u7B49\u5F85\u4F60\u7684\u4E0B\u4E00\u4E2A\u547D\u4EE4\u3002\
  \u8FD9\u91CC\u6709\u4E00\u4E2A\u4F7F\u7528 Bash \u4F5C\u4E3A REPL \u7684\u4F8B\u5B50\
  \uFF1A."
lastmod: '2024-03-13T22:44:47.963450-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Bash \u4E2D\uFF0C\u4F60\u7684\u7EC8\u7AEF\u672C\u8D28\u4E0A\u662F\
  \u4E00\u4E2A REPL\u3002\u4F60\u8F93\u5165\u547D\u4EE4\uFF1B\u5B83\u8BFB\u53D6\u5B83\
  \uFF0C\u6C42\u503C\uFF0C\u6253\u5370\u7ED3\u679C\uFF0C\u5E76\u5FAA\u73AF\u56DE\u7B49\
  \u5F85\u4F60\u7684\u4E0B\u4E00\u4E2A\u547D\u4EE4\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\
  \u4F7F\u7528 Bash \u4F5C\u4E3A REPL \u7684\u4F8B\u5B50\uFF1A."
title: "\u5728\u7F16\u7A0B\u4E2D\u4F7F\u7528\u4EA4\u4E92\u5F0FShell\uFF08REPL\uFF09"
weight: 34
---

## 如何操作：
在 Bash 中，你的终端本质上是一个 REPL。你输入命令；它读取它，求值，打印结果，并循环回等待你的下一个命令。这里有一个使用 Bash 作为 REPL 的例子：

```Bash
$ echo "Hello, World!"
Hello, World!
$ x=$((6 * 7))
$ echo $x
42
```

你的输入遵循 `$ ` 提示符，输出在下一行打印。简单，对吧？

## 深入了解
Bash，简称 Bourne Again SHell，是许多基于 Unix 的系统上的默认 shell。它是对最初在 1970 年代末构建的 Bourne shell 的升级。虽然 Bash 是一个强大的脚本工具，但其交互模式允许你逐行执行命令。

考虑到替代品，你有 Python REPL（只需在终端中输入 `python`），Node.js（使用 `node`），以及 IPython，一个增强的交互式 Python shell。每种语言往往都有自己的 REPL 实现。

在底层，REPLs 是循环，解析你的输入（命令或代码），运行它，并将结果返回到 stdout（你的屏幕），通常直接使用语言的解释器。这种反馈的即时性非常适合学习和原型设计。

## 另请参阅
- [官方 GNU Bash 文档](https://gnu.org/software/bash/manual/bash.html)
- [学习 Shell 交互式教程](https://www.learnshell.org/)
- [IPython 官方网站](https://ipython.org/)
- [REPL.it](https://replit.com/)：一个多语言在线 REPL（不仅仅是 Bash！）
