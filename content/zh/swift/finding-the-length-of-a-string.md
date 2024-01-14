---
title:                "Swift: 查找字符串的长度"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 为什么

在Swift编程中，字符串（string）是一种常见的数据类型，通常用于存储文本。但是，在处理字符串时，我们经常需要知道它们的长度。知道字符串的长度有许多好处，例如可以帮助我们正确地格式化文本或者进行字符串比较。因此，了解如何计算字符串的长度是很重要的。

# 如何计算

要计算字符串的长度，我们可以使用Swift内置的`count`方法。它的使用非常简单，只需将要计算长度的字符串放在方法的括号中即可。例如：

```Swift
let str = "Swift编程"
print(str.count)

// Output: 4
```

我们也可以将`count`方法与字符串的`characters`属性一起使用。`characters`属性是一个字符串中每个字符的集合，我们可以使用`count`方法计算该集合的长度，从而得到字符串的长度。代码示例如下：

```Swift
let str = "¡Hola!"
print(str.characters.count)

// Output: 6
```

# 深入探讨

在Swift中，字符串的长度和它的`UnicodeScalars`属性有关。`UnicodeScalars`是一种表示字符的统一编码方式，它可以将不同语言和符号统一表示成数字。在计算字符串的长度时，Swift会根据每个字符所占用的`UnicodeScalar`数量来计算长度。例如，一个汉字所占用的`UnicodeScalar`数量为一个，而一个英文字母所占用的`UnicodeScalar`数量为一个。因此，在计算字符串长度时，我们也要考虑不同语言和符号的特殊性。

# 参考链接

- [Swift官方文档-字符串和字符](https://swiftgg.gitbook.io/swift/swift-jiao-cheng/03_strings_and_characters)
- [如何在Swift中计算字符串长度](https://www.swiftbysundell.com/posts/string-length-in-swift)
- [字符串处理的基本技巧](https://www.hackingwithswift.com/articles/175/advanced-swift-how-to-use-string-interpolation-in-swift)
- [Swift中的Unicode编码](https://www.cnswift.org/unicode)


# 参见

[Swift常用字符串操作方法](https://www.jianshu.com/p/dfa161b290f2)