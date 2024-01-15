---
title:                "提取子字符串"
html_title:           "Swift: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么
如果你需要在字符串中找出特定的一部分内容，可能会遇到需要提取子字符串的情况。比如，在一个长的网址中，你可能只需要获取其中的域名部分。提取子字符串就是解决这类问题的最佳方法。

## 如何提取子字符串
提取子字符串可以通过使用`substring`方法来实现。以下是一个简单的示例，假设我们有一个字符串`let myString = "Hello, world!"`，现在我们想要提取其中的“world”部分。

```
Swift let myString = "Hello, world!"
let subString = myString.substring(from: 7)
print(subString) // 输出：world
```

在上面的代码中，我们首先定义了一个字符串`myString`，然后使用`substring`方法并传入参数7来提取从第7个字符开始的子字符串，即“world”部分。最后，通过`print`语句打印出提取的子字符串。

## 深入了解提取子字符串
除了上面提到的使用`substring`方法提取子字符串外，还有其他方法可以实现相同的功能。例如，我们可以使用下标来获取字符串中的特定字符，然后将它们拼接起来：

```
Swift let myString = "Hello, world!"
var subString = ""

for i in 7..<12 {
  subString += String(myString[i])
}
print(subString) // 输出：world
```

在上面的代码中，我们使用一个循环来遍历从第7个字符到第11个字符，然后将每个字符转换为字符串并拼接起来。最终得到的结果仍然是“world”。

## 参考链接
- [Swift字符串文档](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [使用Swift提取子字符串](https://www.hackingwithswift.com/quick-start/understanding-swift/how-to-extract-a-substring-from-a-string)
- [在Swift中提取子字符串的不同方法](https://medium.com/@gabemanford/how-to-extract-a-substring-in-swift-987d1a684f2a)

## 参见
- [Swift字符串操作指南](https://www.biaodianfu.com/swift-string-handling-guide.html)
- [如何在Swift中使用字符串](https://www.appcoda.com/swift-string/)