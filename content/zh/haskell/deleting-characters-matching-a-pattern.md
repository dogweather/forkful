---
title:                "删除匹配模式的字符"
html_title:           "Java: 删除匹配模式的字符"
simple_title:         "删除匹配模式的字符"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 什么和为什么？

删除符合模式的字符是指从字符串中剔除满足某种条件的一类字符。这么做是因为我们经常需要处理信息，而字符串正是信息的常见载体。

## 如何操作

Haskell中，我们可以使用`filter`函数结合`notElem`来完成这个操作。示例如下：

```Haskell
deletePattern :: String -> String -> String
deletePattern pattern str = filter (`notElem` pattern) str
```

在这个函数中，`deletePattern`接受两个字符串。第一个是字符集，第二个是要处理的字符串。输出就是一个没有包含第一个字符串中的任何字符的第二个字符串。

如果我们输入代码：

```Haskell
main = print $ deletePattern "aie" "haskell"
```

输出结果会是：

```Haskell
"hskll"
```

## 深度挖掘

这种字符串处理的方式在很早以前就已经非常常见。通常，你可以在人类计算明晰的历史之初，像 Unix 工具 `tr` 这类的地方找到这种想法。此外，Haskell 提供了大量的工具进行字符串处理，如完全函数来简化这个操作。

从实现细节来看，`filter`函数其实就是一个过滤器。它迭代处理一个列表，并用谓词函数筛选出每个元素。而 `notElem` 函数正是我们的谓词函数，它告诉`filter`什么时候保留一个元素，什么时候舍弃。

## 延伸阅读

如果你对这个话题感兴趣，这里有一些相关的资源供你参考：

- Haskell 文档中关于 [filter](https://hackage.haskell.org/package/base/docs/Prelude.html#v:filter) 的部分就能对它的工作方式有一个深入的理解。

- Unix `tr` 命令的 [维基百科](https://zh.wikipedia.org/wiki/Tr_%28Unix%29) 词条也包含了大量历史背景和命令例子。

- 最后，如果你想更多的了解 Haskell 如何处理字符串，可以看看关于 [Data.Text](https://hackage.haskell.org/package/text/docs/Data-Text.html) 库的介绍。