---
date: 2024-01-26 01:38:10.191009-07:00
description: "\u600E\u4E48\u505A\uFF1A \u5047\u8BBE\u4F60\u6709\u4E00\u5927\u5757\
  \ Haskell \u4EE3\u7801\uFF0C\u91CD\u590D\u81EA\u5DF1\u8D85\u8FC7\u4F60\u6700\u559C\
  \u6B22\u7684\u6B4C\u7684\u6B21\u6570\u3002\u8FD9\u91CC\u5FEB\u901F\u770B\u4E00\u4E0B\
  \u4F60\u53EF\u80FD\u5982\u4F55\u4F7F\u7528\u51FD\u6570\u8FDB\u884C\u91CD\u6784\u3002\
  \ \u91CD\u6784\u524D\uFF1A."
lastmod: '2024-04-05T22:38:46.986680-06:00'
model: gpt-4-0125-preview
summary: "\u600E\u4E48\u505A\uFF1A \u5047\u8BBE\u4F60\u6709\u4E00\u5927\u5757 Haskell\
  \ \u4EE3\u7801\uFF0C\u91CD\u590D\u81EA\u5DF1\u8D85\u8FC7\u4F60\u6700\u559C\u6B22\
  \u7684\u6B4C\u7684\u6B21\u6570\u3002\u8FD9\u91CC\u5FEB\u901F\u770B\u4E00\u4E0B\u4F60\
  \u53EF\u80FD\u5982\u4F55\u4F7F\u7528\u51FD\u6570\u8FDB\u884C\u91CD\u6784\u3002 \u91CD\
  \u6784\u524D\uFF1A."
title: "\u91CD\u6784\u4EE3\u7801"
weight: 19
---

## 怎么做：
假设你有一大块 Haskell 代码，重复自己超过你最喜欢的歌的次数。这里快速看一下你可能如何使用函数进行重构。

重构前：

```haskell
printInvoice :: String -> Float -> String -> IO ()
printInvoice customer total item = do
  putStrLn $ "Customer: " ++ customer
  putStrLn $ "Total: " ++ show total
  putStrLn $ "Item: " ++ item
```

经过一点重构后：

```haskell
printDetail :: String -> String -> IO ()
printDetail label value = putStrLn $ label ++ ": " ++ value

printInvoice :: String -> Float -> String -> IO ()
printInvoice customer total item = do
  printDetail "Customer" customer
  printDetail "Total" (show total)
  printDetail "Item" item

-- 示例输出：
-- Customer: Alice
-- Total: $42.00
-- Item: Haskell 编程指南
```

如你所见，通过将通用模式提取到一个单独的 `printDetail` 函数中，我们避免了重复，并使得 `printInvoice` 更清晰、更易于管理。

## 深入探讨
Haskell 在 80 年代末出现时，很明显，函数式范式可以为编程实践带来一些新鲜空气。时间快进，由于函数是一等公民且具有强静态类型系统，Haskell 中的重构特别优雅。你在重构时不必担心会破坏你的应用，因为编译器会保护你。

手动重构的替代方法可能包括使用自动化工具，尽管与其他语言相比，Haskell 的函数式本质和类型安全有时使这种情况不太普遍。实现方面，重要的是利用 Haskell 的特性，如高阶函数、纯度和不变性，使重构更加顺畅。

像刚刚展示的“提取函数”这样的重构很常见，但得益于类型系统，你也可以自信地进行“内联函数”、“重命名变量”和“更改函数签名”。Haskell 强大的类型推断有时可以捕捉到在其他语言中可能会被忽视的错误。

## 参见
想要深入了解 Haskell 中的重构，请查阅 Martin Fowler 的《重构：改善既有代码的设计》，其中的概念普遍适用。查看 hlint 工具以自动获取关于改善你的 Haskell 代码的提示。此外，浏览 Haskell wiki (https://wiki.haskell.org/Refactoring) 获取社区洞见和进一步阅读。
