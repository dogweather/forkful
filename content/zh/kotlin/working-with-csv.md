---
title:                "与csv编程。"
html_title:           "Kotlin: 与csv编程。"
simple_title:         "与csv编程。"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## 关于什么 & 为什么？
将CSV技术与Kotlin结合，可以方便地处理以逗号分隔的数据表格。程序员经常使用CSV来导入和导出数据，从而使数据更容易管理和分析。

## 如何：
Kotlin中的CSV处理非常简便。首先，将所需的包导入项目中。接着，使用```kotlinx-csv```库来读取和写入CSV文件。下面是一个读取CSV文件并打印数据的代码示例：
```
val csvFile = File("sample.csv").readText()
val data = csvFile.reader().readAll()

for (row in data) {
  row.forEach { println(it) }
}
```
输出结果如下所示：
```
Name, Age, Gender
Alice, 25, Female
Bob, 30, Male
Cindy, 28, Female
```
要将数据写入CSV文件，可以使用以下代码：
```
val data = arrayOf(arrayOf("Name", "Age", "Gender"), arrayOf("David", "35", "Male"))
val csvFile = File("new_sample.csv").bufferedWriter()

data.forEach{row -> 
  csvFile.append(row.joinToString(","))
  csvFile.appendLine()
}

csvFile.close()
```
新的CSV文件的内容如下：
```
Name, Age, Gender
David, 35, Male
```

## 深入探讨：
CSV即Comma Separated Values，它是一种常见的文本文件格式，用于存储和传输表格数据。CSV最初是为电子表格程序设计的，但现在也被广泛用于数据交换。虽然CSV是一种简单易用的格式，但它也有一些缺点，例如不支持复杂的数据类型和结构。此外，也有其他可替代的数据格式，例如JSON和XML。

要在Kotlin中使用CSV，可以选择使用不同的库，例如```common-csv```和```kotlin-csv```。这些库提供了不同的功能和API，可以根据项目需求进行选择。

## 查看更多：
- [kotlinx-csv](https://github.com/doyaaaaaken/kotlin-csv)
- [使用Kotlin和CSV进行数据处理](https://blog.mindorks.com/using-kotlin-and-csv-for-processing-data)
- [Comma-separated values (CSV) format](https://en.wikipedia.org/wiki/Comma-separated_values)