---
title:                "字符串的插值"
html_title:           "Swift: 字符串的插值"
simple_title:         "字符串的插值"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 什么&为什么？
插值字符串是一种在 Swift 中使用变量和表达式来构建字符串的方法。程序员使用它来方便地将数据插入到字符串中，从而减少重复的代码和提高可读性。

## 如何实现：
下面是一个简单的示例，展示了如何使用插值字符串来构建一个打招呼的消息： 
```Swift
let name = "小明"
let greeting = "你好，\(name)！欢迎来到 Swift 世界。"
print(greeting)
```
输出结果将是： 
```
你好，小明！欢迎来到 Swift 世界。
```

## 深入了解：
插值字符串的概念可以追溯到 C 语言，被称为 "格式化字符串"。在 Swift 中，还有另一种类似的方法，使用 `String(format: String, arguments: [CVarArg])` 函数来实现。但插值字符串相比之下更加简洁易懂，并且能直接在字符串中嵌入变量名。在实现上，Swift 编译器将插值字符串转换为普通的字符串拼接操作，因此性能并不会受到影响。

## 参考资料：
了解更多关于字符串的内容，可以参考苹果官方文档：https://developer.apple.com/documentation/swift/string