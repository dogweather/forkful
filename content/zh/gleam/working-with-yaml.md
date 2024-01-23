---
title:                "处理 YAML 文件"
html_title:           "Bash: 处理 YAML 文件"
simple_title:         "处理 YAML 文件"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

# What & Why? 什么以及为什么?

YAML是一种数据序列化格式，程序员用它来配置文件因为它易读易写。因为它清晰和简洁，它经常被用来管理项目设置和数据交换。

# How to: 怎么做:

```gleam
// Gleam当前不直接支持YAML解析，需要通过外部库或工具。
// 这里是通过假想的gleam-yaml库的示例代码.

import gleam/yaml

pub fn parse_yaml() {
  let yaml_str = "title: Hello World\nversion: 1.0"
  let result = yaml.to_map(yaml_str)
  case result {
    Ok(data) -> io.println(data) // 应该输出: "title: 'Hello World', version: '1.0'"
    Error(err) -> io.println("解析错误: " ++ err)
  }
}

pub fn generate_yaml() {
  let data = map.from_list([
    ("title", "你好世界"),
    ("version", "2.0"),
  ])
  let yaml_str = yaml.from_map(data)
  io.println(yaml_str) // 应该输出: "title: 你好世界\nversion: 2.0"
}
```

# Deep Dive 深入研究

YAML起源于2001年，旨在成为XML的简化版本。尽管JSON在某些情况下也是一个选项，YAML的人类可读性使它更适合配置文件。Gleam暂时没有官方的YAML处理库，但可以利用Erlang或Elixir的库进行处理。

# See Also 另请参阅

- YAML官网: [https://yaml.org](https://yaml.org)
- Gleam官方文档: [https://gleam.run](https://gleam.run)
- Elixir的YAML库: [https://hex.pm/packages/yaml_elixir](https://hex.pm/packages/yaml_elixir)
