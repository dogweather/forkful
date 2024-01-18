---
title:                "从字符串中解析日期"
html_title:           "Swift: 从字符串中解析日期"
simple_title:         "从字符串中解析日期"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 什么是日期字符串解析？为什么程序员要这么做？

日期字符串解析是将一个日期信息从字符串格式转换成程序可识别的日期对象的过程。程序员会在处理用户输入或从数据库中读取数据时遇到日期字符串，因此需要将其转换为程序能够读取和操作的格式。

## 如何进行日期字符串解析：

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd"
let stringDate = "2021-05-21"
let date = dateFormatter.date(from: stringDate)
print(date)
```

输出结果为：
<code>Optional(2021-05-21 00:00:00 +0000)</code>

## 深入了解：

日期字符串解析在计算机编程历史中是一个重要的技术，它使得读取和处理日期更加便捷。除了使用DateFormatter的方法，还可以通过正则表达式来解析日期字符串，但这种方法相对复杂且容易出错。另外，不同语言和框架都有自己的日期解析方式，因此需要根据不同的情况进行选择。

## 参考资料：

- [Apple官方文档-String Formatting and Parsing](https://developer.apple.com/documentation/foundation/nsformatter)
- [维基百科-Date and time notation](https://en.wikipedia.org/wiki/Date_and_time_notation)
- [Swift文档-String](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)