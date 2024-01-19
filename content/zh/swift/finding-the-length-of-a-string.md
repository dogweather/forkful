---
title:                "查找字符串的长度"
html_title:           "Javascript: 查找字符串的长度"
simple_title:         "查找字符串的长度"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
获取字符串长度是操作程序中字符串变量的一种方式，通过识别计算字符数量来实现。程序员这么做是为了在必要时有效控制和操作字符数据。

## 如何实现？
在Swift中，我们使用String的count属性来获取字符串长度。以下是代码示例和输出：

```Swift
let myString = "编程是有趣的"
print("字符串长度: \(myString.count)")
```
输出：
```Swift
字符串长度: 6
```
这段代码首先定义了一个名为myString的字符串，然后使用.count属性找到并打印字符串的长度。

## 深入浅出
获取字符串长度这个概念不是Swift特有的，它在很多其他编程语言中也有应用。例如，在Python中，我们用len()函数， 而在Java和Javascript中，我们用.length属性。

请注意，关于计算字符的方式，Swift处理字符串的方式略有不同。标准的".count"方法在Swift中返回的是具有扩展字形集群的字符数量（例如：附带变音记号的字母被视为单个字符），而不是单独的Unicode标量数量。这与其他一些编程语言（例如JavaScript）有所不同，在JavaScript中,".length"属性返回的是UTF-16代码单元的数量。

在Swift中，你也可以使用utf16.count或utf8.count属性获取字符串的字节长度，但记住，这与人类可读的文字长度可能会有所不同。

## 参见
对于更深入的信息，请参考以下链接：
- Swift Docs: [Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Unicode Info: [Unicode Character Count](http://unicode.org/glossary/#character)
- StackOverflow: [Length of a String in Swift](https://stackoverflow.com/questions/24092884/what-is-the-length-of-a-string-in-swift)