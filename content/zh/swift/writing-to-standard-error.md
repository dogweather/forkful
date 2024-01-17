---
title:                "向标准错误写入"
html_title:           "Swift: 向标准错误写入"
simple_title:         "向标准错误写入"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 什么 & 为什么？
写入标准错误是将程序输出信息写入到标准错误流中而不是标准输出流的行为。程序员通常会这样做是为了调试和记录重要信息，因为标准错误流会在程序发生错误时显示出来。

## 怎么做：
```
Swift.print("这是标准输出")
Swift.fprint(stderr, "这是标准错误")
```

输出：
```
这是标准输出
这是标准错误
```

## 深入了解：
1. 历史背景：写入标准错误的概念起源于早期计算机，它是一种标准化的输出方式，旨在增加程序的可读性和可维护性。
2. 替代方案：除了使用标准错误流，程序员还可以使用日志记录器或调试器来记录重要信息。
3. 实现细节：在Swift中，我们可以使用`Swift.fprint()`函数来将信息写入标准错误流。

## 参考链接：
- [Swift文档](https://docs.swift.org/swift-book/LanguageGuide/AdvancedOperators.html#ID45)
- [标准错误流的历史发展](https://en.wikipedia.org/wiki/Standard_error)