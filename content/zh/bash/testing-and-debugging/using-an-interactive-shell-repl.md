---
date: 2024-01-26 04:11:33.875138-07:00
description: "REPL \u4EE3\u8868\u8BFB\u53D6-\u6C42\u503C-\u6253\u5370\u5FAA\u73AF\uFF0C\
  \u8FD9\u662F\u4E00\u4E2A\u7B80\u5355\u7684\u4EA4\u4E92\u5F0F\u8BA1\u7B97\u673A\u7F16\
  \u7A0B\u73AF\u5883\u3002\u7F16\u7801\u8005\u4F7F\u7528\u5B83\u6765\u5FEB\u901F\u7F16\
  \u5199\u548C\u6D4B\u8BD5\u4EE3\u7801\uFF0C\u5B9E\u9A8C\u8BED\u6CD5\uFF0C\u5E76\u5728\
  \u4E0D\u9700\u8981\u521B\u5EFA\u548C\u8FD0\u884C\u6574\u4E2A\u5E94\u7528\u7A0B\u5E8F\
  \u7684\u60C5\u51B5\u4E0B\u5B66\u4E60\u7F16\u7A0B\u6982\u5FF5\u3002"
lastmod: '2024-03-13T22:44:47.963450-06:00'
model: gpt-4-0125-preview
summary: "REPL \u4EE3\u8868\u8BFB\u53D6-\u6C42\u503C-\u6253\u5370\u5FAA\u73AF\uFF0C\
  \u8FD9\u662F\u4E00\u4E2A\u7B80\u5355\u7684\u4EA4\u4E92\u5F0F\u8BA1\u7B97\u673A\u7F16\
  \u7A0B\u73AF\u5883\u3002\u7F16\u7801\u8005\u4F7F\u7528\u5B83\u6765\u5FEB\u901F\u7F16\
  \u5199\u548C\u6D4B\u8BD5\u4EE3\u7801\uFF0C\u5B9E\u9A8C\u8BED\u6CD5\uFF0C\u5E76\u5728\
  \u4E0D\u9700\u8981\u521B\u5EFA\u548C\u8FD0\u884C\u6574\u4E2A\u5E94\u7528\u7A0B\u5E8F\
  \u7684\u60C5\u51B5\u4E0B\u5B66\u4E60\u7F16\u7A0B\u6982\u5FF5\u3002"
title: "\u5728\u7F16\u7A0B\u4E2D\u4F7F\u7528\u4EA4\u4E92\u5F0FShell\uFF08REPL\uFF09"
---

{{< edit_this_page >}}

## 什么 & 为什么？
REPL 代表读取-求值-打印循环，这是一个简单的交互式计算机编程环境。编码者使用它来快速编写和测试代码，实验语法，并在不需要创建和运行整个应用程序的情况下学习编程概念。

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
