---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:18.979024-07:00
description: "\u5982\u4F55\u505A\uFF1A \u9ED8\u8BA4\u60C5\u51B5\u4E0B\uFF0CSwift \u4E0D\
  \u5305\u542B\u7528\u4E8E HTML \u89E3\u6790\u7684\u5185\u7F6E\u5E93\uFF0C\u8FD9\u5C31\
  \u9700\u8981\u4F7F\u7528\u7B2C\u4E09\u65B9\u5E93\u6765\u6709\u6548\u5730\u5904\u7406\
  \u8FD9\u9879\u4EFB\u52A1\u3002\u5176\u4E2D\u6700\u53D7\u6B22\u8FCE\u7684\u9009\u62E9\
  \u4E4B\u4E00\u662F SwiftSoup\uFF0C\u4E00\u4E2A\u7EAF Swift \u5E93\uFF0C\u5B83\u63D0\
  \u4F9B\u4E86\u7C7B\u4F3C jQuery \u7684\u8BED\u6CD5\u6765\u8FDB\u884C HTML \u89E3\
  \u6790\u548C\u64CD\u4F5C\u3002"
lastmod: '2024-04-05T21:53:48.445719-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u89E3\u6790HTML"
weight: 43
---

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
