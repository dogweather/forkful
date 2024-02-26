---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:40.458526-07:00
description: "\u5904\u7406 CSV\uFF08\u9017\u53F7\u5206\u9694\u503C\uFF09\u6587\u4EF6\
  \u6D89\u53CA\u5230\u4ECE\u6BCF\u884C\u4EE3\u8868\u4E00\u4E2A\u8BB0\u5F55\u3001\u6BCF\
  \u4E2A\u8BB0\u5F55\u4E2D\u7684\u5B57\u6BB5\u7531\u9017\u53F7\u5206\u9694\u7684\u6587\
  \u672C\u6587\u4EF6\u4E2D\u89E3\u6790\u548C\u751F\u6210\u7ED3\u6784\u5316\u6570\u636E\
  \u3002\u7A0B\u5E8F\u5458\u5E38\u5E38\u4ECE\u4E8B\u8FD9\u9879\u6D3B\u52A8\uFF0C\u4EE5\
  \u4FBF\u901A\u8FC7\u4E00\u79CD\u5E7F\u6CDB\u652F\u6301\u4E0D\u540C\u5E73\u53F0\u548C\
  \u7F16\u7A0B\u8BED\u8A00\u7684\u683C\u5F0F\uFF0C\u8F7B\u677E\u5730\u5BFC\u5165\u3001\
  \u5BFC\u51FA\u548C\u64CD\u63A7\u8868\u683C\u6570\u636E\uFF0C\u56E0\u4E3A\u5B83\u7684\
  \u7B80\u5355\u6027\u548C\u6613\u8BFB\u6027\u3002"
lastmod: '2024-02-25T18:49:45.750609-07:00'
model: gpt-4-0125-preview
summary: "\u5904\u7406 CSV\uFF08\u9017\u53F7\u5206\u9694\u503C\uFF09\u6587\u4EF6\u6D89\
  \u53CA\u5230\u4ECE\u6BCF\u884C\u4EE3\u8868\u4E00\u4E2A\u8BB0\u5F55\u3001\u6BCF\u4E2A\
  \u8BB0\u5F55\u4E2D\u7684\u5B57\u6BB5\u7531\u9017\u53F7\u5206\u9694\u7684\u6587\u672C\
  \u6587\u4EF6\u4E2D\u89E3\u6790\u548C\u751F\u6210\u7ED3\u6784\u5316\u6570\u636E\u3002\
  \u7A0B\u5E8F\u5458\u5E38\u5E38\u4ECE\u4E8B\u8FD9\u9879\u6D3B\u52A8\uFF0C\u4EE5\u4FBF\
  \u901A\u8FC7\u4E00\u79CD\u5E7F\u6CDB\u652F\u6301\u4E0D\u540C\u5E73\u53F0\u548C\u7F16\
  \u7A0B\u8BED\u8A00\u7684\u683C\u5F0F\uFF0C\u8F7B\u677E\u5730\u5BFC\u5165\u3001\u5BFC\
  \u51FA\u548C\u64CD\u63A7\u8868\u683C\u6570\u636E\uFF0C\u56E0\u4E3A\u5B83\u7684\u7B80\
  \u5355\u6027\u548C\u6613\u8BFB\u6027\u3002"
title: "\u5904\u7406CSV\u6587\u4EF6"
---

{{< edit_this_page >}}

## 什么 & 为什么？

处理 CSV（逗号分隔值）文件涉及到从每行代表一个记录、每个记录中的字段由逗号分隔的文本文件中解析和生成结构化数据。程序员常常从事这项活动，以便通过一种广泛支持不同平台和编程语言的格式，轻松地导入、导出和操控表格数据，因为它的简单性和易读性。

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
