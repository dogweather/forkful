---
title:                "处理 CSV 文件"
html_title:           "Bash: 处理 CSV 文件"
simple_title:         "处理 CSV 文件"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
CSV，即逗号分隔值，是数据存储的简单格式。程序员用它因为其简易且广泛支持，适合数据交换和处理。

## How to: (如何操作：)
```Ruby
require 'csv'

# 创建CSV文件
CSV.open("example.csv", "wb") do |csv|
  csv << ["姓名", "年龄", "城市"]
  csv << ["王小明", 25, "北京"]
  csv << ["张小华", 30, "上海"]
end

# 读取CSV文件
CSV.foreach("example.csv") do |row|
  puts row.inspect
end

# 输出：
# ["姓名", "年龄", "城市"]
# ["王小明", 25, "北京"]
# ["张小华", 30, "上海"]

# 解析CSV字符串
data = CSV.parse("姓名,年龄,城市\n王小明,25,北京\n张小华,30,上海")
puts data.inspect

# 输出：
# [["姓名", "年龄", "城市"], ["王小明", "25", "北京"], ["张小华", "30", "上海"]]
```

## Deep Dive (深入探讨)
CSV在1972年被IBM推广，现成为最通用的数据交换格式之一。用Ruby的CSV库相对于JSON、XML有时更方便，特别是处理表格数据时。Ruby内置库`csv`提供读写CSV文件的功能，不需额外gem。实现细节包括支持不同编码、自定义分隔符和引号字符等。

## See Also (另请参阅)
- Ruby CSV文档：[https://ruby-doc.org/stdlib-3.0.0/libdoc/csv/rdoc/CSV.html](https://ruby-doc.org/stdlib-3.0.0/libdoc/csv/rdoc/CSV.html)
- FasterCSV gem，Ruby老版本的高速CSV处理库：[https://rubygems.org/gems/fastercsv](https://rubygems.org/gems/fastercsv)
- RFC 4180，CSV标准：[https://tools.ietf.org/html/rfc4180](https://tools.ietf.org/html/rfc4180)
