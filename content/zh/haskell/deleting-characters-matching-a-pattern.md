---
title:                "删除符合模式的字符"
html_title:           "Haskell: 删除符合模式的字符"
simple_title:         "删除符合模式的字符"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
删除匹配模式的字符是指从一个字符串中移除特定的字符组合。程序员常常会这样做来处理字符串中不必要的字符，从而使字符串更加清晰和可读。

## 如何：
Haskell中删除匹配模式的字符可以使用函数`filter`。该函数接受一个谓词函数作为输入，并返回该谓词函数为`True`的元素组成的新列表。下面是一个示例代码及其输出，展示如何使用该函数来删除字符串中的所有数字和标点符号：
```Haskell
str = "Hello, 1 world!"
output = filter (\x -> not (x `elem` ['0'..'9'] ++ [' ','!'])) str
```

输出结果为`"Hello,world"`。在这个例子中，我们使用谓词函数`\x -> not (x `elem` ['0'..'9'] ++ [' ','!'])`来判断输入的字符是否为数字或标点符号，如果不是，则该字符被保留在输出结果中。

## 深入：
历史上，删除匹配模式的字符是通过遍历字符串并逐个比对字符来实现的。但是，这种做法效率较低且容易出错。Haskell中的`filter`函数提供了一种更加简洁高效的实现方式。除了使用`filter`函数，程序员还可以使用正则表达式来实现删除匹配模式的字符。正则表达式是一种强大的模式匹配工具，可以更灵活地处理字符串中的字符。

## 看也：
- [Haskell文档中关于`filter`函数的解释](https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:filter)
- [正则表达式在Haskell中的应用](https://www.haskell.org/haskellwiki/Regular_expressions)