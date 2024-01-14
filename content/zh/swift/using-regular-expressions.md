---
title:                "Swift: 使用正则表达式"
simple_title:         "使用正则表达式"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

为什么：使用正则表达式的原因只有1-2句话。

随着移动应用程序和网站的不断发展，数据处理变得越来越重要。正则表达式是一种强大的工具，可以帮助程序员有效地处理和匹配文本。无论是验证用户输入，还是提取特定文本，正则表达式都可以帮助提高编程效率。

## 如何使用

要在Swift中使用正则表达式，首先需要引入正则表达式库。

```Swift
import Foundation
```

接下来，我们可以使用`NSRegularExpression`类来创建一个正则表达式对象，其中包含了我们想要匹配的模式。

```Swift
let regex = try? NSRegularExpression(pattern: "[a-zA-Z]+", options: .caseInsensitive)
```

现在，我们可以使用`matches`方法来检查一个字符串是否符合我们的模式，并返回相应的结果。

```Swift
let result = regex.matches(in: "Hello World!", options: [], range: NSMakeRange(0, string.count))
```

如果匹配成功，`result`中将会包含相关信息，如匹配文本的位置和范围等。我们可以通过循环遍历这些结果，来获取匹配的文本。

```Swift
for match in result {
  let range = match.range
  let matchedText = (string as NSString).substring(with: range)
  print(matchedText)
}
```

输出将会是：

```
Hello
World
```

## 深入了解

除了简单的模式匹配，正则表达式还具有许多强大的功能。例如，我们可以使用捕获组来提取特定部分的文本。捕获组是在模式中用括号括起来的部分，我们可以通过在匹配结果中使用`range(at:)`方法来获取这些部分的位置和范围。

```Swift
let regex = try? NSRegularExpression(pattern: "(\d+)-(\d+)", options: [])
let result = regex.matches(in: "2021-08-19", options: [], range: NSMakeRange(0, string.count))[0]
let yearRange = result.range(at: 1)
let monthRange = result.range(at: 2)
let year = (string as NSString).substring(with: yearRange)
let month = (string as NSString).substring(with: monthRange)
print("Year: \(year), Month: \(month)")
```

输出将会是：

```
Year: 2021, Month: 08
```

此外，正则表达式还具有一些特殊的元字符，如`\d`表示匹配数字，`\w`表示匹配字母、数字和下划线等。程序员可以结合这些元字符和其他特殊字符，来创建自己需要的复杂模式。

## 参考文献

- [NSRegularExpression - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Regular Expressions Cheat Sheet - Mac OS X Hints](https://s3.amazonaws.com/mcp-files/resources/1674/regular-expressions-cheat-sheet-v2.pdf)
- [Regular Expressions Tutorial - Ray Wenderlich](https://www.raywenderlich.com/227593/regular-expressions-tutorial-swift-part-1)
- [Swift Oneliner: Use regular expression in Swift - Swift Blog](https://www.swiftbysundell.com/posts/swift-oneliner-using-a-regular-expression)

## 参考链接

- [使用正则表达式进行字符串匹配](https://www.jianshu.com/p/785f189a093f)
- [Swift Regular Expressions- 菜鸟教程](https://www.runoob.com/w3cnote/swift-regular-expressions.html)
- [用Swift实现字符串模式匹配- SegmentFault](https://segmentfault.com/a/1190000002908568)