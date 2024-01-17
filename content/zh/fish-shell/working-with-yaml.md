---
title:                "使用yaml进行编程"
html_title:           "Fish Shell: 使用yaml进行编程"
simple_title:         "使用yaml进行编程"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## 什么是YAML & 为什么程序员需要它？
YAML是一种使用简单易读的语法结构来表示数据的格式。它可以作为配置文件或者存储数据的方式。程序员会使用YAML来组织和存储数据，使其易于阅读和修改。

## 如何操作？
```fish shell
# 创建一个YAML文件
touch mydata.yaml

# 使用Fish Shell的微型文本编辑器来编辑数据
fish_config

# 在YAML文件中定义数据，例如：
name: Alice
age: 25
hobbies:
- reading
- hiking
```

## 深入了解
YAML是"YAML Ain't Markup Language"的递归缩写，是一种由Clark Evans和Ingy döt Net共同开发的数据序列化格式。它的设计目的是要比其他格式更易于使用和阅读。除了YAML，还有其他类似的格式，例如JSON和XML，但它们并不像YAML那样易于阅读和修改。

## 查看更多
- [YAML官方网站](https://yaml.org/)
- [YAML语法指南](https://www.cloudbees.com/blog/yaml-tutorial-everything-you-need-get-started/)