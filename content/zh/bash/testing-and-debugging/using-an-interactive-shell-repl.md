---
title:                "在编程中使用交互式Shell（REPL）"
aliases:
- /zh/bash/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:11:33.875138-07:00
model:                 gpt-4-0125-preview
simple_title:         "在编程中使用交互式Shell（REPL）"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/using-an-interactive-shell-repl.md"
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
