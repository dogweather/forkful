---
title:                "使用JSON进行编程"
aliases:
- /zh/ruby/working-with-json/
date:                  2024-02-03T19:23:59.259296-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用JSON进行编程"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么?

JSON（JavaScript对象表示法）是一种轻量级的数据交换格式，在客户端和服务器之间的数据交换的web应用程序中非常普遍。程序员使用Ruby处理JSON，以解析来自外部来源的数据或格式化数据以供API响应，利用其易于阅读的结构来轻松地进行数据操作和存储。

## 如何操作:

Ruby通过其标准库，提供了解析和生成JSON的无缝方法。这些操作的主要模块是`json`，可以轻松地集成到任何Ruby应用程序中。

### 解析JSON:

要将JSON字符串转换为Ruby哈希，您可以使用`JSON.parse`方法。

```ruby
require 'json'

json_string = '{"name": "John Doe", "age": 30, "city": "New York"}'
ruby_hash = JSON.parse(json_string)

puts ruby_hash
# 输出: {"name"=>"John Doe", "age"=>30, "city"=>"New York"}
```

### 生成JSON:

相反，要将Ruby哈希转换为JSON字符串，您使用`JSON.generate`方法或在需要`json`库后可用于Ruby对象的`to_json`方法。

```ruby
require 'json'

ruby_hash = { name: "Jane Doe", age: 25, city: "Los Angeles" }
json_string = ruby_hash.to_json

puts json_string
# 输出: {"name":"Jane Doe","age":25,"city":"Los Angeles"}
```

### 第三方库:

虽然Ruby的标准库覆盖了基本的JSON处理，但许多项目依赖于第三方库，以获得增强的功能和性能。一个受欢迎的选择是`Oj`（优化的JSON）。

#### 使用Oj解析:

```ruby
require 'oj'

json_string = '{"name": "Alex", "age": 40, "city": "Chicago"}'
ruby_hash = Oj.load(json_string)

puts ruby_hash
# 输出: {"name"=>"Alex", "age"=>40, "city"=>"Chicago"}
```

#### 使用Oj生成:

Oj还提供了一种快速从Ruby对象生成JSON的方法：

```ruby
require 'oj'

ruby_hash = { name: "Samantha", age: 35, city: "Miami" }
json_string = Oj.dump(ruby_hash)

puts json_string
# 输出: {"name":"Samantha","age":35,"city":"Miami"}
```

这些例子说明了在Ruby中处理JSON的直接性，使其适用于从简单的数据操作到复杂的API通信的任务。
