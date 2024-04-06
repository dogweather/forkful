---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:11.580180-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Ruby \u9ED8\u8BA4\u5305\u542B CSV \u5E93\
  \uFF0C\u8FD9\u7B80\u5316\u4E86\u4ECE CSV \u6587\u4EF6\u8BFB\u53D6\u548C\u5199\u5165\
  \u7684\u64CD\u4F5C\u3002\u4EE5\u4E0B\u662F\u5982\u4F55\u5229\u7528\u8FD9\u4E00\u70B9\
  \u6765\u5B8C\u6210\u5E38\u89C1\u4EFB\u52A1\uFF1A \u8981\u4ECE CSV \u6587\u4EF6\u8BFB\
  \u53D6\uFF0C\u4F60\u9996\u5148\u9700\u8981\u5F15\u5165 CSV \u5E93\u3002\u7136\u540E\
  \uFF0C\u4F60\u53EF\u4EE5\u8FED\u4EE3\u884C\u6216\u5C06\u5B83\u4EEC\u8BFB\u5165\u6570\
  \u7EC4\u3002"
lastmod: '2024-03-13T22:44:48.399345-06:00'
model: gpt-4-0125-preview
summary: "Ruby \u9ED8\u8BA4\u5305\u542B CSV \u5E93\uFF0C\u8FD9\u7B80\u5316\u4E86\u4ECE\
  \ CSV \u6587\u4EF6\u8BFB\u53D6\u548C\u5199\u5165\u7684\u64CD\u4F5C\u3002\u4EE5\u4E0B\
  \u662F\u5982\u4F55\u5229\u7528\u8FD9\u4E00\u70B9\u6765\u5B8C\u6210\u5E38\u89C1\u4EFB\
  \u52A1\uFF1A\n\n\u8981\u4ECE CSV \u6587\u4EF6\u8BFB\u53D6\uFF0C\u4F60\u9996\u5148\
  \u9700\u8981\u5F15\u5165 CSV \u5E93\u3002\u7136\u540E\uFF0C\u4F60\u53EF\u4EE5\u8FED\
  \u4EE3\u884C\u6216\u5C06\u5B83\u4EEC\u8BFB\u5165\u6570\u7EC4."
title: "\u5904\u7406CSV\u6587\u4EF6"
weight: 37
---

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
