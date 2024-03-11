---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:11.580180-07:00
description: "\u5728 Ruby \u4E2D\u5904\u7406 CSV \u6587\u4EF6\u63D0\u4F9B\u4E86\u4E00\
  \u79CD\u76F4\u63A5\u5904\u7406\u8868\u683C\u6570\u636E\u7684\u65B9\u6CD5\u3002\u7A0B\
  \u5E8F\u5458\u7ECF\u5E38\u4ECE\u4E8B\u8FD9\u9879\u5B9E\u8DF5\uFF0C\u7528\u4E8E\u6570\
  \u636E\u89E3\u6790\u3001\u63D0\u53D6\u3001\u8F6C\u6362\u548C\u5B58\u50A8\uFF0C\u4F7F\
  \u5176\u6210\u4E3A\u6D89\u53CA\u6570\u636E\u64CD\u7EB5\u6216\u5206\u6790\u4EFB\u52A1\
  \u7684\u5173\u952E\u6280\u80FD\u3002"
lastmod: '2024-03-11T00:14:22.212938-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Ruby \u4E2D\u5904\u7406 CSV \u6587\u4EF6\u63D0\u4F9B\u4E86\u4E00\u79CD\
  \u76F4\u63A5\u5904\u7406\u8868\u683C\u6570\u636E\u7684\u65B9\u6CD5\u3002\u7A0B\u5E8F\
  \u5458\u7ECF\u5E38\u4ECE\u4E8B\u8FD9\u9879\u5B9E\u8DF5\uFF0C\u7528\u4E8E\u6570\u636E\
  \u89E3\u6790\u3001\u63D0\u53D6\u3001\u8F6C\u6362\u548C\u5B58\u50A8\uFF0C\u4F7F\u5176\
  \u6210\u4E3A\u6D89\u53CA\u6570\u636E\u64CD\u7EB5\u6216\u5206\u6790\u4EFB\u52A1\u7684\
  \u5173\u952E\u6280\u80FD\u3002"
title: "\u5904\u7406CSV\u6587\u4EF6"
---

{{< edit_this_page >}}

## 什么 & 为什么？

在 Ruby 中处理 CSV 文件提供了一种直接处理表格数据的方法。程序员经常从事这项实践，用于数据解析、提取、转换和存储，使其成为涉及数据操纵或分析任务的关键技能。

## 如何操作：

Ruby 默认包含 CSV 库，这简化了从 CSV 文件读取和写入的操作。以下是如何利用这一点来完成常见任务：

### 读取 CSV 文件
要从 CSV 文件读取，你首先需要引入 CSV 库。然后，你可以迭代行或将它们读入数组。

```ruby
require 'csv'

# 将每一行读作一个数组
CSV.foreach("data.csv") do |row|
  puts row.inspect
end

# 每行的输出可能看起来像这样：["data1", "data2", "data3"]
```

### 写入 CSV
写入 CSV 文件也很直接。你可以向现有文件追加内容或创建新文件进行写入。

```ruby
require 'csv'

CSV.open("output.csv", "wb") do |csv|
  csv << ["header1", "header2", "header3"]
  csv << ["value1", "value2", "value3"]
end

# 这会创建或覆盖 'output.csv'，并使用指定的标题和值。
```

### 解析 CSV 字符串
有时你需要直接从字符串解析 CSV 数据。以下是如何做到这一点：

```ruby
require 'csv'

data = "name,age,city\nJohn Doe,29,New York\nJane Doe,31,Chicago"
csv = CSV.parse(data, headers: true)

csv.each do |row|
  puts "#{row['name']} - #{row['age']} - #{row['city']}"
end

# 预期输出：
# John Doe - 29 - New York
# Jane Doe - 31 - Chicago
```

### 使用 SmarterCSV
对于更复杂的 CSV 任务，`SmarterCSV` gem 可以是一个有价值的工具。首先，安装 gem：

```shell
gem install smarter_csv
```

然后，你可以使用它来处理大文件或执行更复杂的解析和操作：

```ruby
require 'smarter_csv'

options = {}
data = SmarterCSV.process('large_data.csv', options)

data.each do |hash|
  puts hash.inspect
end

# 这将读取 'large_data.csv' 并根据标题将每行输出为一个 hash。
```

总而言之，Ruby 的内置 CSV 库，连同第三方 gem 如 `SmarterCSV`，提供了强大的支持来处理 CSV 数据，允许有效地进行数据处理和操作任务。
