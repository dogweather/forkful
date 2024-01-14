---
title:    "Swift: 串接字符串"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## 为什么要连接字符串

当我们需要将多个字符串组合成一个时，就需要用到字符串连接。这种方法可以让我们更方便地处理字符串，并且可以使我们的代码更加简洁。

## 如何连接字符串

要连接两个字符串，可以使用"+"运算符。例如：

```Swift
let str1 = "Hello"
let str2 = "world"
let combinedStr = str1 + str2
```

输出结果：HelloWorld

除了使用"+"运算符，我们还可以使用字符串插值来连接字符串。这样可以将变量或常量直接插入到字符串中。例如：

```Swift
let name = "Jay"
let greeting = "Hello, \(name)!"
```

输出结果：Hello, Jay!

当我们需要连接多个字符串时，我们可以使用字符串的`.joined()`方法。这个方法接受一个数组作为参数，并将数组中的每个字符串组合成一个大字符串。例如：

```Swift
let array = ["I", "love", "programming"]
let sentence = array.joined(separator: " ")
```

输出结果：I love programming

## 深入了解连接字符串

在Swift中，字符串是一种值类型，意味着它们每次被修改时都会产生一个新的字符串。因此，当我们使用"+"运算符连接字符串时，实际上是创建了一个新的字符串，并将原有的字符串拼接在一起。这也意味着连接字符串的操作会消耗更多的内存，因此在处理大量字符串时需要注意内存使用情况。

另外，我们也可以使用Swift的`.appending()`方法来连接字符串。与"+"运算符不同的是，这个方法会直接在原有字符串的末尾插入新的字符串，并返回一个新的字符串。这可以帮助我们节省一些内存空间。

## 参考链接

- [Apple官方文档：字符串和字符](https://developer.apple.com/documentation/swift/string_and_character)
- [Swift中字符串的连接与插值](https://www.jianshu.com/p/ae55e3681754)
- [谈谈Swift中字符串连接操作的性能消耗](https://zhuanlan.zhihu.com/p/29343983)

## 查看更多

- [使用字符串插值的Swift语言基础](https://blog.csdn.net/u014205508/article/details/54179260)
- [Swift中字符串的基本操作](https://www.jianshu.com/p/0cffd276b204)
- [如何高效地拼接多个字符串](https://juejin.im/post/5c7f5b6d6fb9a04a037b7b67)