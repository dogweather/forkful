---
date: 2024-01-20 17:43:00.965007-07:00
description: "\u5220\u9664\u5339\u914D\u6A21\u5F0F\u7684\u5B57\u7B26\u662F\u6307\u5728\
  \u5B57\u7B26\u4E32\u4E2D\u627E\u5230\u5E76\u79FB\u9664\u7B26\u5408\u7279\u5B9A\u89C4\
  \u5219\u7684\u5B57\u7B26\u3002\u7F16\u7A0B\u65F6\u8FD9\u4E48\u505A\u53EF\u4EE5\u5E2E\
  \u52A9\u6E05\u6D17\u6570\u636E\u3001\u79FB\u9664\u65E0\u5173\u5185\u5BB9\uFF0C\u4F7F\
  \u5B57\u7B26\u4E32\u66F4\u52A0\u5E72\u51C0\u3001\u4FBF\u4E8E\u5904\u7406\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:45.707558-07:00'
model: gpt-4-1106-preview
summary: "\u5220\u9664\u5339\u914D\u6A21\u5F0F\u7684\u5B57\u7B26\u662F\u6307\u5728\
  \u5B57\u7B26\u4E32\u4E2D\u627E\u5230\u5E76\u79FB\u9664\u7B26\u5408\u7279\u5B9A\u89C4\
  \u5219\u7684\u5B57\u7B26\u3002\u7F16\u7A0B\u65F6\u8FD9\u4E48\u505A\u53EF\u4EE5\u5E2E\
  \u52A9\u6E05\u6D17\u6570\u636E\u3001\u79FB\u9664\u65E0\u5173\u5185\u5BB9\uFF0C\u4F7F\
  \u5B57\u7B26\u4E32\u66F4\u52A0\u5E72\u51C0\u3001\u4FBF\u4E8E\u5904\u7406\u3002"
title: "\u5339\u914D\u6A21\u5F0F\u5220\u9664\u5B57\u7B26"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么要这么做？)
删除匹配模式的字符是指在字符串中找到并移除符合特定规则的字符。编程时这么做可以帮助清洗数据、移除无关内容，使字符串更加干净、便于处理。

## How to: (怎么做：)
在Swift中，你可以使用`removeAll(where:)`来删除字符：

```Swift
var greeting = "Hello, World! Where are the cookies?"
greeting.removeAll(where: { "aeiou".contains($0) })
print(greeting)
// 输出: Hll, Wrld! Whr r th ckks?
```

或者使用正则表达式来删除符合模式的字符串：

```Swift
var stringWithNumbers = "My phone number is 123-456-7890."
if let range = stringWithNumbers.range(of: "\\d{3}-\\d{3}-\\d{4}", options: .regularExpression) {
    stringWithNumbers.removeSubrange(range)
}
print(stringWithNumbers)
// 输出: My phone number is .
```

## Deep Dive (深入探究)
删除匹配模式的字符在编程中并不新鲜。在早期计算机科学中，正则表达式便广泛用于模式匹配。Swift简化了某些常见的任务比如`removeAll(where:)`，但对于复杂模式，正则表达式依然是强大工具。除了`removeAll`和正则表达式，你也可以通过循环遍历字符串并使用`StringBuilder`或者数组拼接的方式来删除字符，但这在Swift中并不常见。考虑到执行效率和代码清晰度，推荐优先使用上面展示的方法。

## See Also (另请参阅)
- Swift标准库文档中的[String](https://developer.apple.com/documentation/swift/string)；
- [正则表达式快速参考](https://www.raywenderlich.com/800-swift-regular-expressions-cheatsheet)；
- 核心字符串处理相关的[NSRegularExpression](https://developer.apple.com/documentation/foundation/nsregularexpression)类。
