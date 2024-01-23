---
title:                "处理 YAML 文件"
html_title:           "Bash: 处理 YAML 文件"
simple_title:         "处理 YAML 文件"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML是一种数据序列化格式，用于配置文件与数据交换。程序员用它因为它易读易写，适合人类阅读和机器解析。

## How to:
使用`PyYAML`库，安装：`pip install PyYAML`。

读取YAML：

```Python
import yaml

# 假设有一个名为example.yaml的文件
with open('example.yaml', 'r') as file:
    data = yaml.safe_load(file)
    print(data)
```

写入YAML：

```Python
import yaml

data = {'key': 'value', 'list': [1, 2, 3]}

with open('example.yaml', 'w') as file:
    yaml.dump(data, file)
```

## Deep Dive
YAML于2001年提出，意为“YAML Ain't Markup Language”。比XML和JSON更易读，支持注释。常见替代有JSON、TOML。PyYAML是Python中解析YAML的主流库，它提供了强大的API来序列化和反序列化YAML数据。

## See Also
- PyYAML文档: https://pyyaml.org/wiki/PyYAMLDocumentation
- YAML官方网站: https://yaml.org
- JSON官方网站: https://www.json.org/json-zh.html
- TOML GitHub仓库: https://github.com/toml-lang/toml
