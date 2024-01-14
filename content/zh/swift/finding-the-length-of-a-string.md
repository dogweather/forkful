---
title:    "Swift: 寻找字符串的长度"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 为什么
在编写程序时，常常需要对字符串的长度进行操作。因此，了解如何找到字符串的长度是非常重要的，在未来的编程工作中会经常用到。

## 如何操作
要找到字符串的长度，可以使用Swift内建的`count`方法。例如，假设我们有一个字符串变量`str`，要找到它的长度，可以在代码中使用以下命令：
```Swift
let length = str.count
```
如果我们想输出这个长度变量的值，可以使用`print`语句，并在括号中放入`length`变量：
```Swift
print(length)
```
使用这个代码段，就可以得到字符串的长度，并将其打印出来。比如，假设变量`str`是一个名字`Bob`，那么输出的结果就会是`3`。

## 深入探讨
字符串的长度表示的是其中的字符数，并不是以字节为单位的。换句话说，一个中文字符的长度并不是一个字节，而是根据Unicode编码所确定的。因此，在处理包含中文字符的字符串时，使用`count`方法可能会得到比实际字符数少的结果。这时，应当考虑使用其他方法来计算长度，比如`NSString`类中的`length`属性。

# 参考链接
- [Swift官方文档-字符串和字符](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [jikejunyi的博客-Swift字符串(String)字符(Character)](https://jikejunyi.github.io/2016/10/16/swift-string/&cd=17&hl=zh-CN&ct=clnk&gl=us&client=safari)