---
title:                "与CSV的工作"
html_title:           "Swift: 与CSV的工作"
simple_title:         "与CSV的工作"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/working-with-csv.md"
---

{{< edit_this_page >}}

# 为什么

CSV（逗号分隔值）是一种普遍用于存储和分享数据的格式。它的简单结构使得它易于理解和处理，因此它被广泛使用。在本文中，我们将介绍如何使用Swift来处理CSV文件。

## 如何做

我们可以使用Swift CSV库来处理CSV文件。首先，我们需要导入CSV库，并加载CSV文件。假设我们有一个叫做“data.csv”的文件，其中包含有关学生的姓名、年龄和成绩的数据。

```Swift
import CSV

do {
    let csv = try CSV(url: "data.csv")
} catch {
    print("无法加载CSV文件")
}
```

接下来，我们可以使用“rows”方法来遍历每一行数据，并使用“columns”方法来访问每一行中的列数据。例如，如果我们想要打印每个学生的成绩，可以使用如下代码：

```Swift
for row in csv.rows {
    if let grade = row["成绩"] {
        print("\(row["姓名"]!)的成绩是\(grade)")
    }
}
```

运行上述代码，我们可以得到类似以下输出：

```
张三的成绩是98
李四的成绩是89
王五的成绩是77
```

除了遍历每一行数据，我们还可以使用“namedRows”方法来根据某个特定的列来访问数据。例如，如果我们想要根据学生的姓名来获取数据，可以使用如下代码：

```Swift
let namedRows = csv.namedRows(columnNames: ["姓名", "年龄", "成绩"])

let zhangSan = namedRows["张三"]
print("\(zhangSan["姓名"])今年\(zhangSan["年龄"])岁，成绩是\(zhangSan["成绩"])")
```

输出将会是：

```
张三今年18岁，成绩是98
```

## 深入了解

除了基本的读取和访问数据之外，CSV库还提供了许多其他有用的功能，如写入数据、更改数据格式、处理空白行等等。您可以查看[官方文档](https://github.com/swiftcsv/SwiftCSV/blob/master/README-zh.md)来深入了解这些功能。

## 另请参阅

- [Swift CSV库官方文档](https://github.com/swiftcsv/SwiftCSV)
- [CSV格式标准](https://tools.ietf.org/html/rfc4180)
- [CSV的历史和用途](https://en.wikipedia.org/wiki/Comma-separated_values)