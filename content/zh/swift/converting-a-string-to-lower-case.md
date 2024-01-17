---
title:                "将字符串转换为小写"
html_title:           "Swift: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

什么是字符串转换成小写？为什么程序员要这样做？

字符串转换为小写是指将大写字母变成小写字母。程序员经常这样做是因为在编写代码时，经常会出现大小写不一致的问题，这会影响程序的运行。

如何进行字符串转换为小写？

```
Swift let exampleString = "Hello World!" print(exampleString.lowercased()) // output: hello world! 
```

深入了解

字符串转换为小写的历史背景：在早期的计算机编程中，只能输入大写字母，随着技术的发展，程序员发现在编写代码时，使用小写字母能够提高可读性和代码的效率。

其他替代方法：除了使用.lowercased()方法之外，程序员还可以使用.uppercased()方法将字符串转换为大写，或者使用.capitalized方法将字符串首字母大写。

实现细节：在Swift中，字符串类型有一个lowercased()方法，可用于将字符串转换为小写。该方法会返回一个新的字符串，而不是修改原字符串。

相关链接

了解更多关于字符串转换为小写的信息，请参考Swift官方文档：https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID290