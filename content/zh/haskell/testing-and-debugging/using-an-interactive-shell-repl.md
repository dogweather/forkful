---
date: 2024-01-26 04:14:58.485518-07:00
description: "Haskell\u4E2D\u7684\u4EA4\u4E92\u5F0Fshell\uFF0C\u6216\u8005\u79F0\u4E3A\
  REPL\uFF08\u8BFB\u53D6-\u6C42\u503C-\u6253\u5370\u5FAA\u73AF\uFF09\uFF0C\u8BA9\u4F60\
  \u80FD\u591F\u5B9E\u65F6\u8FD0\u884C\u4EE3\u7801\u7247\u6BB5\u3002\u5B83\u662F\u4E00\
  \u4E2A\u7528\u4E8E\u5FEB\u901F\u53CD\u9988\u3001\u6D4B\u8BD5\u51FD\u6570\u548C\u5B66\
  \u4E60\u8BED\u8A00\u7684\u6E38\u4E50\u573A\u3002"
lastmod: 2024-02-19 22:05:06.856307
model: gpt-4-0125-preview
summary: "Haskell\u4E2D\u7684\u4EA4\u4E92\u5F0Fshell\uFF0C\u6216\u8005\u79F0\u4E3A\
  REPL\uFF08\u8BFB\u53D6-\u6C42\u503C-\u6253\u5370\u5FAA\u73AF\uFF09\uFF0C\u8BA9\u4F60\
  \u80FD\u591F\u5B9E\u65F6\u8FD0\u884C\u4EE3\u7801\u7247\u6BB5\u3002\u5B83\u662F\u4E00\
  \u4E2A\u7528\u4E8E\u5FEB\u901F\u53CD\u9988\u3001\u6D4B\u8BD5\u51FD\u6570\u548C\u5B66\
  \u4E60\u8BED\u8A00\u7684\u6E38\u4E50\u573A\u3002"
title: "\u5728\u7F16\u7A0B\u4E2D\u4F7F\u7528\u4EA4\u4E92\u5F0FShell\uFF08REPL\uFF09"
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
