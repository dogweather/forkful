---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:59.259296-07:00
description: "\u5982\u4F55\u64CD\u4F5C: Ruby\u901A\u8FC7\u5176\u6807\u51C6\u5E93\uFF0C\
  \u63D0\u4F9B\u4E86\u89E3\u6790\u548C\u751F\u6210JSON\u7684\u65E0\u7F1D\u65B9\u6CD5\
  \u3002\u8FD9\u4E9B\u64CD\u4F5C\u7684\u4E3B\u8981\u6A21\u5757\u662F`json`\uFF0C\u53EF\
  \u4EE5\u8F7B\u677E\u5730\u96C6\u6210\u5230\u4EFB\u4F55Ruby\u5E94\u7528\u7A0B\u5E8F\
  \u4E2D\u3002"
lastmod: '2024-04-05T21:53:48.670745-06:00'
model: gpt-4-0125-preview
summary: "Ruby\u901A\u8FC7\u5176\u6807\u51C6\u5E93\uFF0C\u63D0\u4F9B\u4E86\u89E3\u6790\
  \u548C\u751F\u6210JSON\u7684\u65E0\u7F1D\u65B9\u6CD5\u3002\u8FD9\u4E9B\u64CD\u4F5C\
  \u7684\u4E3B\u8981\u6A21\u5757\u662F`json`\uFF0C\u53EF\u4EE5\u8F7B\u677E\u5730\u96C6\
  \u6210\u5230\u4EFB\u4F55Ruby\u5E94\u7528\u7A0B\u5E8F\u4E2D\u3002"
title: "\u4F7F\u7528JSON\u8FDB\u884C\u7F16\u7A0B"
weight: 38
---

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
