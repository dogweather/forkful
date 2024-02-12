---
title:                "使用TOML"
aliases:
- zh/python/working-with-toml.md
date:                  2024-01-26T04:25:52.199371-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/working-with-toml.md"
---

{{< edit_this_page >}}

## 是什么 & 为什么？
TOML，代表Tom's Obvious, Minimal Language（汤姆的明显、最简语言），是一种类似于JSON或YAML的数据序列化格式，但它旨在简单和易读。程序员使用TOML进行配置文件编写，因为它易于编写和理解，并且能够整齐地映射到像Python这样的编程语言中的数据结构上。

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
