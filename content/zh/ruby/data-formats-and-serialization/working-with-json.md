---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:59.259296-07:00
description: "JSON\uFF08JavaScript\u5BF9\u8C61\u8868\u793A\u6CD5\uFF09\u662F\u4E00\
  \u79CD\u8F7B\u91CF\u7EA7\u7684\u6570\u636E\u4EA4\u6362\u683C\u5F0F\uFF0C\u5728\u5BA2\
  \u6237\u7AEF\u548C\u670D\u52A1\u5668\u4E4B\u95F4\u7684\u6570\u636E\u4EA4\u6362\u7684\
  web\u5E94\u7528\u7A0B\u5E8F\u4E2D\u975E\u5E38\u666E\u904D\u3002\u7A0B\u5E8F\u5458\
  \u4F7F\u7528Ruby\u5904\u7406JSON\uFF0C\u4EE5\u89E3\u6790\u6765\u81EA\u5916\u90E8\
  \u6765\u6E90\u7684\u6570\u636E\u6216\u683C\u5F0F\u5316\u6570\u636E\u4EE5\u4F9BAPI\u54CD\
  \u5E94\uFF0C\u5229\u7528\u5176\u6613\u4E8E\u9605\u8BFB\u7684\u7ED3\u6784\u6765\u8F7B\
  \u677E\u5730\u8FDB\u884C\u6570\u636E\u64CD\u4F5C\u548C\u5B58\u50A8\u3002"
lastmod: '2024-03-11T00:14:22.211448-06:00'
model: gpt-4-0125-preview
summary: "JSON\uFF08JavaScript\u5BF9\u8C61\u8868\u793A\u6CD5\uFF09\u662F\u4E00\u79CD\
  \u8F7B\u91CF\u7EA7\u7684\u6570\u636E\u4EA4\u6362\u683C\u5F0F\uFF0C\u5728\u5BA2\u6237\
  \u7AEF\u548C\u670D\u52A1\u5668\u4E4B\u95F4\u7684\u6570\u636E\u4EA4\u6362\u7684web\u5E94\
  \u7528\u7A0B\u5E8F\u4E2D\u975E\u5E38\u666E\u904D\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\
  Ruby\u5904\u7406JSON\uFF0C\u4EE5\u89E3\u6790\u6765\u81EA\u5916\u90E8\u6765\u6E90\
  \u7684\u6570\u636E\u6216\u683C\u5F0F\u5316\u6570\u636E\u4EE5\u4F9BAPI\u54CD\u5E94\
  \uFF0C\u5229\u7528\u5176\u6613\u4E8E\u9605\u8BFB\u7684\u7ED3\u6784\u6765\u8F7B\u677E\
  \u5730\u8FDB\u884C\u6570\u636E\u64CD\u4F5C\u548C\u5B58\u50A8\u3002"
title: "\u4F7F\u7528JSON\u8FDB\u884C\u7F16\u7A0B"
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
