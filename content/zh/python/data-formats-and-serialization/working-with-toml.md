---
date: 2024-01-26 04:25:52.199371-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728\u5F00\u59CB\u4E4B\u524D\uFF0C\u4F7F\
  \u7528`pip install toml`\u5B89\u88C5`toml`\u5305\u3002\u8BA9\u6211\u4EEC\u89E3\u6790\
  \u4E00\u4E2ATOML\u6587\u4EF6\uFF1A."
lastmod: '2024-04-05T21:53:47.634480-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u4F7F\u7528TOML"
weight: 39
---

## 如何操作：
在开始之前，使用`pip install toml`安装`toml`包。让我们解析一个TOML文件：

```python
import toml

# 字符串形式的示例TOML内容
toml_string = """
[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z # 一级日期

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
"""

# 解析TOML字符串
parsed_toml = toml.loads(toml_string)

# 访问数据
print(parsed_toml['owner']['name'])  # 输出：Tom Preston-Werner
print(parsed_toml['database']['ports'])  # 输出：[8001, 8001, 8002]
```

## 深入探究
TOML由GitHub的联合创始人之一Tom Preston-Werner创建，作为一种更友好的配置文件格式。它旨在无歧义地映射到哈希表，并且可以被机器轻松解析。

与JSON相比，TOML在配置文件中更具可读性，并支持注释。YAML是另一种选择，可能更紧凑，但它依赖于缩进，以及像不允许使用制表符这样的细微问题，可能会使人困惑。

就实现细节而言，TOML的值是有类型的，包括字符串、整数、浮点数、布尔值、日期时间、数组和表格。一切都是区分大小写的。此外，TOML支持多行字符串，并且在最新版本中，甚至允许异构类型数组。

Python使用的是`toml`库，它在API方面与JSON和YAML库相仿。你有`toml.load`和`toml.loads`用于从文件或字符串读取TOML，以及`toml.dump`和`toml.dumps`用于写出它。

## 另请参阅
- 官方TOML GitHub仓库了解规范：[github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- `toml` Python库文档：[pypi.org/project/toml/](https://pypi.org/project/toml/)
- TOML的现实世界示例：Rust的包管理器`cargo`或Python打包工具`poetry`的配置文件。
