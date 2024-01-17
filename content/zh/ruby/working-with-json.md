---
title:                "使用JSON进行编程"
html_title:           "Ruby: 使用JSON进行编程"
simple_title:         "使用JSON进行编程"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## 使用JSON：为什么以及如何？

JSON是一种通用的文件格式，用于存储和交换数据。它的简洁性和易读性使得它成为程序员最喜欢的文件格式之一。许多编程语言都内置了对JSON的支持，包括最新版本的Ruby（2.5.0）。程序员们通常使用JSON来存储和传输结构化数据，例如配置文件、API响应和数据库查询结果。

## 如何使用JSON？

使用JSON的最简单方法是使用Ruby的内置类JSON。首先，您需要导入JSON库，在您的程序开头添加“require 'json'”。然后，您可以使用```Ruby JSON.parse()```和```Ruby JSON.generate()```方法来解析和生成JSON数据。

### 解析JSON数据

要解析JSON数据并将其转换为Ruby对象，您可以使用```Ruby JSON.parse()```方法。看下面的例子：

```
require 'json'
json = '{"name": "John", "age": 30, "country": "USA"}'
user = JSON.parse(json)
puts user
```

输出将会是：

```
{"name"=>"John", "age"=>30, "country"=>"USA"}
```

如您所见，JSON数据已经被转换为Ruby hash对象。您可以使用惯用的Ruby方法来访问和修改这些数据。

### 生成JSON数据

要根据Ruby对象生成JSON数据，您可以使用```Ruby JSON.generate()```方法。看下面的例子：

```
require 'json'
user = {"name"=>"John", "age"=>30, "country"=>"USA"}
json = JSON.generate(user)
puts json
```

输出将会是：

```
{"name": "John","age": 30,"country": "USA"}
```

请注意，JSON数据的键将会以双引号表示，这是JSON格式的要求。

## 深入了解JSON

JSON最初由Douglas Crockford于2001年设计。它的设计目标是替代JavaScript中复杂的对象表示。在过去的几年中，JSON已经成为Web应用程序和API之间常用的数据交换格式。JSON也有一些竞争对手，例如XML和YAML，但由于JSON的简洁性和易读性，它已经成为最受欢迎的选择。

如果您想要更深入地了解JSON，您可以查看JSON官方网站（json.org），或者阅读更多关于Ruby对JSON的支持的文档。

## 相关链接

[Built-in JSON Library in Ruby](https://ruby-doc.org/stdlib-2.5.0/libdoc/json/rdoc/JSON.html)