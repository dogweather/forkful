---
title:                "将字符串转换为小写"
html_title:           "Arduino: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 什么和为什么？
将字符串转换为小写是让字符串中的所有字符都以小写形式出现的过程。程序员之所以这样做，是因为在比较文本数据等操作中，将所有字符统一为小写可以避免大小写带来的问题，使得数据处理更方便。

# 怎么做：
在Swift中，你可以使用`lowercased()`方法来将字符串转换为小写。这是一个简单的例子：

```Swift
let str = "Hello, Swift!"
let lowercasedStr = str.lowercased()
print(lowercasedStr)  //"hello, swift!"
```

在这个例子中，我们首先定义了一个字符串`str`，然后使用`lowercased()`方法将其转换为全小写，并赋值给`lowercasedStr`。最后，我们打印出`lowercasedStr`的值，你会看到所有的字符都被转换为小写了。

# 深度挖掘：
字符串小写化是一种常见的文本处理操作，并且几乎所有的编程语言都提供了相应的支持。在早期的编程语言中，例如C，程序员需要自己编写函数来完成这个操作。然而，现代编程语言，如Swift,提供了内置的方法来简化这个过程。

另外，Python是另一种提供类似功能的编程语言，它使用的方法是`lower()`。同时，JavaScript也有相似的功能，使用的方法是`toLowerCase()`。

具体到字符串转小写这个功能的实现，其实在底层上跟字符编码有关。每个大写字母在ASCII表中都有一个对应的小写字符，他们之间的区别就是ASCII编码值的差异。

# 参考资料：
为了更深入的理解字符串小写化，你也可以参考以下的链接：
- Swift官方文档：[https://swift.org/documentation/#the-swift-programming-language](https://swift.org/documentation/#the-swift-programming-language)
- Stack Overflow上关于字符串小写化的讨论：[https://stackoverflow.com/questions/26318577/how-do-i-convert-to-lowercase-in-swift](https://stackoverflow.com/questions/26318577/how-do-i-convert-to-lowercase-in-swift)