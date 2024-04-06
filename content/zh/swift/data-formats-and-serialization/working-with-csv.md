---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:40.458526-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 Swift \u4E2D\uFF0C\u6CA1\u6709\
  \u76F4\u63A5\u89E3\u6790 CSV \u6587\u4EF6\u7684\u539F\u751F\u652F\u6301\uFF0C\u4F46\
  \u4F60\u53EF\u4EE5\u901A\u8FC7\u4F7F\u7528 `String` \u65B9\u6CD5\u5206\u5272\u5185\
  \u5BB9\u6216\u5229\u7528\u7B2C\u4E09\u65B9\u5E93\u4F8B\u5982 SwiftCSV \u6765\u5904\
  \u7406 CSV \u6570\u636E\uFF0C\u4EE5\u83B7\u5F97\u66F4\u6D41\u7545\u7684\u65B9\u5F0F\
  \u3002\u8FD9\u91CC\u6709\u4E24\u79CD\u65B9\u6CD5\uFF1A."
lastmod: '2024-04-05T22:38:47.331712-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 Swift \u4E2D\uFF0C\u6CA1\u6709\u76F4\
  \u63A5\u89E3\u6790 CSV \u6587\u4EF6\u7684\u539F\u751F\u652F\u6301\uFF0C\u4F46\u4F60\
  \u53EF\u4EE5\u901A\u8FC7\u4F7F\u7528 `String` \u65B9\u6CD5\u5206\u5272\u5185\u5BB9\
  \u6216\u5229\u7528\u7B2C\u4E09\u65B9\u5E93\u4F8B\u5982 SwiftCSV \u6765\u5904\u7406\
  \ CSV \u6570\u636E\uFF0C\u4EE5\u83B7\u5F97\u66F4\u6D41\u7545\u7684\u65B9\u5F0F\u3002\
  \u8FD9\u91CC\u6709\u4E24\u79CD\u65B9\u6CD5\uFF1A."
title: "\u5904\u7406CSV\u6587\u4EF6"
weight: 37
---

## 如何操作：
在 Swift 中，没有直接解析 CSV 文件的原生支持，但你可以通过使用 `String` 方法分割内容或利用第三方库例如 SwiftCSV 来处理 CSV 数据，以获得更流畅的方式。这里有两种方法：

### 不使用外部库的手动解析
```swift
// 考虑一个简单的 CSV 字符串
let csvString = """
name,age,city
John Doe,29,New York
Jane Smith,34,Los Angeles
"""

// 将 CSV 字符串分割为行
let rows = csvString.components(separatedBy: "\n")

// 从第一行提取键
let keys = rows.first?.components(separatedBy: ",")

// 从第二行开始迭代所有行
var result: [[String: String]] = []
for row in rows.dropFirst() {
    let values = row.components(separatedBy: ",")
    let dict = Dictionary(uniqueKeysWithValues: zip(keys!, values))
    result.append(dict)
}

// 示例输出
print(result)
// 输出：[{"city": "New York", "age": "29", "name": "John Doe"}, {"city": "Los Angeles", "age": "34", "name": "Jane Smith"}]
```
这种方法直接明了但缺乏健全性，特别是处理包含值中的逗号、字段内的换行等特殊情况的 CSV 文件时。

### 使用 SwiftCSV 库
首先，通过在你的 `Package.swift` 依赖中包含 SwiftCSV 来向项目中添加 SwiftCSV：
```swift
.package(url: "https://github.com/swiftcsv/SwiftCSV.git", from: "0.5.6")
```
然后，如下导入并使用它：
```swift
import SwiftCSV

// 假设 `csvString` 如上所定义

// 创建一个 CSV 对象
if let csv = try? CSV(string: csvString) {
    // 以字典形式访问行
    let rows = csv.namedRows
    
    // 示例输出
    print(rows)
    // 输出：[{"city": "New York", "age": "29", "name": "John Doe"}, {"city": "Los Angeles", "age": "34", "name": "Jane Smith"}]
}
```
SwiftCSV 通过自动处理封装的逗号、字段中的换行和字符编码等细节问题简化了解析工作。然而，记住在现实应用中处理可能出现的错误，尤其是处理外部数据源时。
