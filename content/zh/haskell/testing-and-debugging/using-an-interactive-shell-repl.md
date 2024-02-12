---
title:                "在编程中使用交互式Shell（REPL）"
aliases:
- /zh/haskell/using-an-interactive-shell-repl/
date:                  2024-01-26T04:14:58.485518-07:00
model:                 gpt-4-0125-preview
simple_title:         "在编程中使用交互式Shell（REPL）"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## 什么以及为什么？
Haskell中的交互式shell，或者称为REPL（读取-求值-打印循环），让你能够实时运行代码片段。它是一个用于快速反馈、测试函数和学习语言的游乐场。

## 如何操作：
要启动GHCi（Glasgow Haskell Compiler的交互式环境），只需在终端中输入`ghci`。以下是如何使用它的方法：

```Haskell
Prelude> let x = 5
Prelude> x * 2
10
Prelude> :t x
x :: Num a => a
```

示例输出解释了`x`是一个数值变量，并显示将其加倍结果为10。

## 深入了解：
自从其诞生起，Haskell的GHCi已经取得了长足的进步。它提供了一系列丰富的功能，如标签补全、多行输入和包装载入。类似Hugs的替代品现在大多是历史性的，GHCi已成为标准。GHCi每次输入表达式时都会即时编译代码，为你测试Haskell代码提供了一种高效的方式。

## 另见：
- [GHC用户指南 - GHCi](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/ghci.html)
- [为大好事学习你的Haskell - 开始](http://learnyouahaskell.com/starting-out#hello-world)
- [Haskell Wiki - GHC/GHCi](https://wiki.haskell.org/GHC/GHCi)
