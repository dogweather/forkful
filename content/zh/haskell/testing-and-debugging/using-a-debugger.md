---
date: 2024-01-26 03:49:59.072153-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8BA9\u6211\u4EEC\u4E00\u8D77\u4F7F\u7528\
  \ GHCi\uFF0C\u8FD9\u662F\u4E00\u4E2A\u4F5C\u4E3A\u57FA\u672C\u8C03\u8BD5\u5668\u7684\
  \ Haskell \u4EA4\u4E92\u5F0F\u73AF\u5883\u3002\u4F60\u7528\u4F60\u7684 Haskell \u4EE3\
  \u7801\u542F\u52A8\u5B83\uFF0C\u5F00\u59CB\u56DB\u5904\u63A2\u7D22\u3002\u8FD9\u91CC\
  \u6709\u4E2A\u4F8B\u5B50\uFF1A."
lastmod: '2024-04-05T22:38:46.982893-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8BA9\u6211\u4EEC\u4E00\u8D77\u4F7F\u7528\
  \ GHCi\uFF0C\u8FD9\u662F\u4E00\u4E2A\u4F5C\u4E3A\u57FA\u672C\u8C03\u8BD5\u5668\u7684\
  \ Haskell \u4EA4\u4E92\u5F0F\u73AF\u5883\u3002\u4F60\u7528\u4F60\u7684 Haskell \u4EE3\
  \u7801\u542F\u52A8\u5B83\uFF0C\u5F00\u59CB\u56DB\u5904\u63A2\u7D22\u3002\u8FD9\u91CC\
  \u6709\u4E2A\u4F8B\u5B50\uFF1A."
title: "\u4F7F\u7528\u8C03\u8BD5\u5668"
weight: 35
---

## 如何操作：
让我们一起使用 GHCi，这是一个作为基本调试器的 Haskell 交互式环境。你用你的 Haskell 代码启动它，开始四处探索。这里有个例子：

```Haskell
main :: IO ()
main = do
    putStrLn "嘿，你叫什么名字？"
    name <- getLine
    putStrLn $ "你好，" ++ name ++ "！让我们来调试。"
    let result = buggyFunction 5
    print result

buggyFunction :: Int -> Int
buggyFunction n = n * 2 -- 假装这里有个错误
```

要开始使用 GHCi 进行调试：

```bash
$ ghci YourHaskellFile.hs
```

在 `buggyFunction` 处设置一个断点：

```Haskell
Prelude> :break buggyFunction
```

运行你的程序：

```Haskell
Prelude> :main
嘿，你叫什么名字？
```

你的程序在 `buggyFunction` 处暂停。现在你可以检查变量、逐步通过代码、评估表达式。

## 深入了解：
历史上，Haskell 因其纯函数和强类型系统而被认为调试工具不那么关键。现实却不同——复杂程序总可以从良好的调试工具中获益。GHCi 提供了基本的调试命令。然而，对于更直观的体验或大规模应用，你可能会探索集成了调试器的 IDE，如带有 Haskell 扩展的 Visual Studio Code 或 IntelliJ 的 Haskell 插件。

调试器的替代品包括使用打印语句，被称为“printf 调试”，或者利用 Haskell 的强类型系统使错误状态无法表示。然而，有时候，没有什么能替代逐步通过代码。

就实现细节而言，Haskell 的调试器与运行时系统一起工作。它可以处理断点、逐步执行和允许变量检查。然而，由于 Haskell 是惰性求值的，事情可能会变得有点非直观。调试 Haskell 程序通常意味着关注表达式何时以及如何被评估。

## 参见：
- [GHC 用户指南 - 调试器](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/debugging.html)
- [IntelliJ Haskell 插件](https://plugins.jetbrains.com/plugin/8258-intellij-haskell)
