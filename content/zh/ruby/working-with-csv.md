---
title:                "使用csv进行编程"
html_title:           "Ruby: 使用csv进行编程"
simple_title:         "使用csv进行编程"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

# 什么是CSV，为什么程序员要用它？
CSV指的是逗号分隔值，是一种轻量级的文件格式，用于存储和交换表格数据。程序员经常使用CSV来导入和导出数据，以及在代码中处理和分析大量数据。

## 如何操作CSV：
```Ruby
# 导入CSV库
require 'csv'

# 从CSV文件中读取数据并打印
CSV.foreach("data.csv") do |row|
   puts row.inspect
end

# 将数据写入CSV文件
CSV.open("output.csv", "w") do |csv|
   csv << ["Name", "Age", "Gender"]
   csv << ["John", "25", "Male"]
   csv << ["Jane", "28", "Female"]
end

# 输出：
# ["Name", "Age", "Gender"]
# ["John", "25", "Male"]
# ["Jane", "28", "Female"]
```

## 深入了解CSV：
CSV最早由Microsoft Excel引入，它的简单结构和易于读写的特性使得它成为处理数据的理想格式。除了Ruby的CSV库，还有一些其他的CSV解析器，如FasterCSV和SmarterCSV等。在处理大量数据时，使用这些解析器能提高程序的性能表现。

## 相关资源：
- [Ruby官方文档 - CSV类](https://ruby-doc.org/stdlib-2.7.1/libdoc/csv/rdoc/CSV.html)
- [FasterCSV GitHub仓库](https://github.com/JEG2/faster_csv)
- [SmarterCSV GitHub仓库](https://github.com/tilo/smarter_csv)