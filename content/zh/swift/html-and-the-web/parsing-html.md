---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:18.979024-07:00
description: "\u89E3\u6790 HTML \u6307\u7684\u662F\u5206\u89E3\u5E76\u89E3\u91CA HTML\
  \ \u5185\u5BB9\u7684\u7ED3\u6784\u7684\u8FC7\u7A0B\uFF0C\u901A\u5E38\u7528\u4E8E\
  \u63D0\u53D6\u7279\u5B9A\u6570\u636E\u6216\u4EE5\u7F16\u7A0B\u65B9\u5F0F\u64CD\u4F5C\
  \u8FD9\u4E9B\u5185\u5BB9\u3002\u7A0B\u5E8F\u5458\u8FDB\u884C HTML \u89E3\u6790\u7684\
  \u76EE\u7684\u5305\u62EC\u7F51\u7EDC\u6293\u53D6\u3001\u6570\u636E\u6316\u6398\u3001\
  \u81EA\u52A8\u5316\u6D4B\u8BD5\u548C\u5185\u5BB9\u8FC1\u79FB\u4EFB\u52A1\uFF0C\u4F7F\
  \u5E94\u7528\u7A0B\u5E8F\u80FD\u591F\u9AD8\u6548\u5730\u4E0E\u7F51\u7EDC\u6587\u6863\
  \u4EA4\u4E92\u548C\u5904\u7406\u3002"
lastmod: '2024-03-13T22:44:48.155234-06:00'
model: gpt-4-0125-preview
summary: "\u89E3\u6790 HTML \u6307\u7684\u662F\u5206\u89E3\u5E76\u89E3\u91CA HTML\
  \ \u5185\u5BB9\u7684\u7ED3\u6784\u7684\u8FC7\u7A0B\uFF0C\u901A\u5E38\u7528\u4E8E\
  \u63D0\u53D6\u7279\u5B9A\u6570\u636E\u6216\u4EE5\u7F16\u7A0B\u65B9\u5F0F\u64CD\u4F5C\
  \u8FD9\u4E9B\u5185\u5BB9\u3002\u7A0B\u5E8F\u5458\u8FDB\u884C HTML \u89E3\u6790\u7684\
  \u76EE\u7684\u5305\u62EC\u7F51\u7EDC\u6293\u53D6\u3001\u6570\u636E\u6316\u6398\u3001\
  \u81EA\u52A8\u5316\u6D4B\u8BD5\u548C\u5185\u5BB9\u8FC1\u79FB\u4EFB\u52A1\uFF0C\u4F7F\
  \u5E94\u7528\u7A0B\u5E8F\u80FD\u591F\u9AD8\u6548\u5730\u4E0E\u7F51\u7EDC\u6587\u6863\
  \u4EA4\u4E92\u548C\u5904\u7406\u3002"
title: "\u89E3\u6790HTML"
weight: 43
---

## 什么 & 为什么？
解析 HTML 指的是分解并解释 HTML 内容的结构的过程，通常用于提取特定数据或以编程方式操作这些内容。程序员进行 HTML 解析的目的包括网络抓取、数据挖掘、自动化测试和内容迁移任务，使应用程序能够高效地与网络文档交互和处理。

## 如何做：
默认情况下，Swift 不包含用于 HTML 解析的内置库，这就需要使用第三方库来有效地处理这项任务。其中最受欢迎的选择之一是 SwiftSoup，一个纯 Swift 库，它提供了类似 jQuery 的语法来进行 HTML 解析和操作。

### 安装
首先，你需要将 SwiftSoup 添加到你的项目中。如果你使用的是 Swift 包管理器，可以在你的 `Package.swift` 依赖中添加它：

```swift
dependencies: [
    .package(url: "https://github.com/scinfu/SwiftSoup.git", from: "2.3.2")
]
```

### 示例：提取 HTML 中的链接
假设你有一个 HTML 文档，并且你想提取所有的链接 (`<a href="...">`)。使用 SwiftSoup，你可以轻松完成这项任务：

```swift
import SwiftSoup

let html = """
<!DOCTYPE html>
<html>
<head>
    <title>示例页面</title>
</head>
<body>
    <p>欢迎访问我们的网站</p>
    <a href="https://example.com/page1">页面 1</a>
    <a href="https://example.com/page2">页面 2</a>
</body>
</html>
"""

do {
    let doc: Document = try SwiftSoup.parse(html)
    let links: Elements = try doc.select("a")
    for link in links.array() {
        let linkHref: String = try link.attr("href")
        let linkText: String = try link.text()
        print("\(linkText) - \(linkHref)")
    }
} catch Exception.Error(let type, let message) {
    print("错误类型: \(type) 信息: \(message)")
} catch {
    print("错误")
}
```

### 示例输出
前面的代码从 HTML 中提取了 URL 及其文本，输出为：

```
页面 1 - https://example.com/page1
页面 2 - https://example.com/page2
```

这个基础示例演示了如何利用 SwiftSoup 解析 HTML 文档。通过进一步探索 SwiftSoup 的文档，你可以找到多种方法来导航、搜索和修改 HTML 内容，使你的 Swift 应用能够轻松处理复杂的网络内容。
