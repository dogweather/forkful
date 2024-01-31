---
title:                "处理 YAML 文件"
date:                  2024-01-19
html_title:           "Bash: 处理 YAML 文件"
simple_title:         "处理 YAML 文件"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
YAML是一种数据序列化格式，用于配置文件和数据交换。程序员处理YAML来简化复杂数据的表示，因为它易读且易于与其他语言兼容。

## How to: (如何操作：)
```Ruby
require 'yaml'

# 创建一些简单的数据
data = {
  "name" => "张三",
  "age" => 30,
  "languages" => ["Ruby", "JavaScript"]
}

# 把数据序列化成YAML格式字符串
yaml_string = data.to_yaml
puts yaml_string

# 把YAML格式字符串反序列化成Ruby对象
loaded_data = YAML.load(yaml_string)
puts loaded_data
```
输出：
```
---
name: 张三
age: 30
languages:
- Ruby
- JavaScript
{"name"=>"张三", "age"=>30, "languages"=>["Ruby", "JavaScript"]}
```

## Deep Dive (深入探讨)
YAML（YAML Ain't Markup Language）起源于2001年，目的是设计一种易于人类阅读的数据序列化格式。与JSON和XML等替代方案相比，YAML的可读性好，且在配置文件中特别受欢迎。Ruby内置了`yaml`库，可以轻松实现YAML数据的加载和解析。Ruby的`YAML.load`能处理复杂的数据结构，如嵌套哈希和数组。

## See Also (另请参阅)
- [YAML 官网](https://yaml.org/)
- [Ruby YAML 模块文档](https://ruby-doc.org/stdlib-3.1.2/libdoc/yaml/rdoc/YAML.html)
