---
title:                "处理 CSV 文件"
date:                  2024-01-19
simple_title:         "处理 CSV 文件"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV，即逗号分隔值，是数据存储的一种简单格式。程序员使用CSV进行数据导入导出，因其简单、兼容性好。

## How to:
```Swift
import Foundation

// 解析CSV数据
func parseCSV(from csvString: String) -> [[String]] {
    // 将字符串按行分割
    let rows = csvString.components(separatedBy: "\n")
    // 为每行数据切割，得到单元格
    return rows.map { $0.components(separatedBy: ",") }
}

// 创建简单的CSV数据
let myCSVString = """
name,age,city
Alice,30,New York
Bob,25,Los Angeles
Charlie,35,Chicago
"""

// 调用解析函数
let parsedCSV = parseCSV(from: myCSVString)
print(parsedCSV)
```

样本输出：
```
[["name", "age", "city"], ["Alice", "30", "New York"], ["Bob", "25", "Los Angeles"], ["Charlie", "35", "Chicago"]]
```

## Deep Dive
CSV起源于20世纪早期，现今是数据交换常用格式。它有诸多替代方案，如JSON、XML，但因为CSV的简洁对于大量扁平数据而言无可比拟。在Swift中，处理CSV时应注意编码问题和逃逸字符，比如引号和换行符。

## See Also
- [RFC 4180](https://tools.ietf.org/html/rfc4180), CSV标准定义
- [Swift Documentation](https://swift.org/documentation/), Swift官方文档
- [Cocoa CSV Parsing Library](https://github.com/yaslab/CSV.swift), 第三方Swift CSV解析库
