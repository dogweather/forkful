---
title:                "使用yaml进行编程"
html_title:           "Ruby: 使用yaml进行编程"
simple_title:         "使用yaml进行编程"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

# YAML操作简介

## 什么&为什么?
YAML（YAML Ain't Markup Language）是一种轻量级的数据序列化格式，可以将结构化数据保存为可读的文本文件。Ruby程序员常使用YAML来存储和读取配置文件和数据结构。

## 如何操作:
下面是一个使用Ruby语言加载和写入YAML数据的示例。

```Ruby
# 加载必要的库
require 'yaml'

# 定义一个Ruby哈希（hash）数据结构
my_hash = {
  name: '小明',
  age: 25,
  hobbies: ['读书', '旅行', '编程']
}

# 将哈希数据转换为YAML格式
yml_data = my_hash.to_yaml

# 将YAML数据写入文件
File.open('my_data.yml', 'w') {|f| f.write yml_data }

# 读取YAML文件并转换为Ruby哈希数据
new_hash = YAML.load_file('my_data.yml')

# 打印哈希数据
p new_hash # 输出: {:name=>"小明", :age=>25, :hobbies=>["读书", "旅行", "编程"]}
```

## 深入了解:
- YAML最初是由Clark Evans和Ingy döt Net在2001年共同开发的，旨在解决XML等标记语言的复杂性问题。
- 除了YAML外，还有其他一些数据序列化格式可用，例如JSON和XML。
- Ruby语言内置了一个YAML库，支持YAML 1.1规范。

## 另请参阅:
- [YAML官方网站](https://yaml.org/)
- [Ruby官方文档-YAML库](https://ruby-doc.org/stdlib-2.7.1/libdoc/yaml/rdoc/YAML.html)