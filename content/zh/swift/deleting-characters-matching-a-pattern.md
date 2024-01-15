---
title:                "删除匹配模式的字符"
html_title:           "Swift: 删除匹配模式的字符"
simple_title:         "删除匹配模式的字符"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 为什么

有时候，我们需要从一个字符串中删除符合特定模式的字符。这可能是为了清理用户输入或者修改文件格式。在这种情况下，Swift提供了一个简单有效的方法来达到这个目的。

## 如何

```Swift 
let inputString = "I love Swift and Mandarin!"

let pattern = "[a-z]" //我们要删除小写字母

let regex = try! NSRegularExpression(pattern: pattern, options: [])

let outputString = regex.stringByReplacingMatches(in: inputString, options: [], range: NSRange(location: 0, length: inputString.utf16.count), withTemplate: "")

print(outputString) //输出为 " ISwiftMandarin!"
```

为了实现删除字符的功能，我们使用了NSRegularExpression类和它的stringByReplacingMatches方法。首先，我们定义了一个输入字符串和一个匹配小写字母的正则表达式。然后，我们使用正则表达式的stringByReplacingMatches方法，将符合正则表达式的字符替换为空字符串，最后输出结果。

## 深入了解

NSRegularExpression是Foundation框架中一个强大的类，它可以通过正则表达式来搜索和替换字符串中的内容。我们可以使用不同的选项来定制和优化正则表达式，比如忽略大小写、多行搜索等。另外，我们也可以使用不同的模板来替换匹配的部分，比如插入特定的字符或者移除匹配的字符。在处理文本和字符串时，正则表达式是一个非常有用的工具，它能帮助我们快速解决一些复杂的问题。

## 查看更多

- [NSRegularExpression文档](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [正则表达式语法介绍](https://developer.apple.com/library/content/documentation/RegularExpression/Conceptual/RegularExpression-Syntax_Reference/Introduction.html#//apple_ref/doc/uid/TP40008190-CH3-SW1)
- [苹果文档中的NSRegularExpression示例](https://developer.apple.com/documentation/foundation/nsregularexpression/examples)