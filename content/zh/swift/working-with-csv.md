---
title:                "处理CSV文件"
aliases:
- zh/swift/working-with-csv.md
date:                  2024-02-03T19:21:40.458526-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理CSV文件"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
