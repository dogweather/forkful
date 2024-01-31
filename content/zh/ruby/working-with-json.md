---
title:                "处理JSON数据"
date:                  2024-01-19
simple_title:         "处理JSON数据"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
什么以及为什么？简单来说，JSON(JavaScript Object Notation)是数据交换的标准格式。因为它简单、易读、跨语言兼容，Ruby程序员常用它来传递和储存数据。

## How to:
如何操作：

```Ruby
require 'json'

# 将Hash转换为JSON字符串
user_info = { name: "张三", age: 30, city: "北京" }
user_info_json = user_info.to_json
puts user_info_json  # => "{\"name\":\"张三\",\"age\":30,\"city\":\"北京\"}"

# 将JSON字符串解析成Ruby的数据结构
received_json = '{"name":"张三","age":30,"city":"北京"}'
parsed_data = JSON.parse(received_json)
puts parsed_data['name']  # => 张三
```

## Deep Dive
深入了解：JSON在2001年由道格拉斯·克罗克福德提出。尽管名为JavaScript对象标记，它与编程语言无关，被多种语言广泛支持，包括Ruby。Ruby的标准库`json`提供了必要的功能。你还可以使用像`oj`（优化的JSON）这样的第三方gem以获得更高效的JSON处理。

## See Also
相关链接：

- Ruby官方文档中的JSON模块: [Ruby JSON](https://ruby-doc.org/stdlib-3.0.0/libdoc/json/rdoc/JSON.html)
- Oj gem：[Oj](https://github.com/ohler55/oj)
- JSON官方网站，含规格和其他语言的信息：[JSON.org](https://www.json.org/json-en.html)
