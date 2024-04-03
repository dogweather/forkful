---
date: 2024-01-26 04:14:58.485518-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8981\u542F\u52A8GHCi\uFF08Glasgow Haskell\
  \ Compiler\u7684\u4EA4\u4E92\u5F0F\u73AF\u5883\uFF09\uFF0C\u53EA\u9700\u5728\u7EC8\
  \u7AEF\u4E2D\u8F93\u5165`ghci`\u3002\u4EE5\u4E0B\u662F\u5982\u4F55\u4F7F\u7528\u5B83\
  \u7684\u65B9\u6CD5\uFF1A."
lastmod: '2024-03-13T22:44:47.817211-06:00'
model: gpt-4-0125-preview
summary: "\u8981\u542F\u52A8GHCi\uFF08Glasgow Haskell Compiler\u7684\u4EA4\u4E92\u5F0F\
  \u73AF\u5883\uFF09\uFF0C\u53EA\u9700\u5728\u7EC8\u7AEF\u4E2D\u8F93\u5165`ghci`\u3002\
  \u4EE5\u4E0B\u662F\u5982\u4F55\u4F7F\u7528\u5B83\u7684\u65B9\u6CD5\uFF1A."
title: "\u5728\u7F16\u7A0B\u4E2D\u4F7F\u7528\u4EA4\u4E92\u5F0FShell\uFF08REPL\uFF09"
weight: 34
---

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
