---
title:    "Swift: 将字符串大写化"
keywords: ["Swift"]
---

{{< edit_this_page >}}

# 为什么要将字符串首字母大写

很多时候，在我们的编程过程中，我们需要对字符串进行格式化，其中之一就是将字符串的首字母大写。这通常是为了让字符串看起来更加美观，也能符合语言的文法规范。在Swift编程中，有几种简单的方法可以实现这一点。

## 如何将字符串首字母大写

方法一：使用capitalized属性

```Swift
let str = "hello world"
print(str.capitalized) // Output: Hello World
```

方法二：使用capitalizingFirstLetter()函数

```Swift
func capitalizingFirstLetter(_ str: String) -> String {
    return str.prefix(1).uppercased() + str.dropFirst()
}

let str = "hello world"
print(capitalizingFirstLetter(str)) // Output: Hello world
```

## 深入了解字符串首字母大写

首先，我们需要了解的是，字符串是一个由字符组成的序列，和数组类似。在Swift中，字符串的每个字符都有一个对应的字符编码，也就是ASCII码，其中包括英文字母、数字、标点符号等。通过这些编码，我们可以实现字符串的各种操作，包括首字母大写。

在方法一中，我们使用了字符串的capitalized属性，其实它是调用了一个名为capitalizingFirstLetter函数来实现的。这个方法会将字符串的首字母变成大写，而其他字符保持不变。如果我们需要将整个字符串的每个单词的首字母都变成大写，可以使用capitalized(with: locale:)方法，其中的locale可以指定不同的语言环境。

方法二中的函数是通过字符串的prefix()和dropFirst()方法来实现的。prefix()方法可以截取字符串的前几个字符，而dropFirst()方法则是去除字符串的前几个字符。结合起来使用，就可以将字符串的首字母提取出来变成大写，再将后面的部分重新拼接起来。

除了 Swift 自带的方法，我们也可以自己实现一个函数来实现字符串首字母大写的功能。通过遍历字符串的每个字符并检查其ASCII码，将小写字母转换成对应的大写字母，就可以实现同样的效果。

## 参考链接

- [Swift 字符串文档](https://swift.org/documentation/)
- [String - Swift Standard Library](https://developer.apple.com/documentation/swift/string)
- [Unicode编码及其应用](https://baike.baidu.com/item/Unicode%E7%BC%96%E7%A0%81%E5%8F%8A%E5%85%B6%E5%BA%94%E7%94%A8/10382638?fr=aladdin)

## 参考代码

[GitHub Gist](https://gist.github.com/) - [Capitalizing First Letter of String in Swift](https://gist.github.com/abbottDev/036c4b5b382be42a55e61f153243725c)