---
date: 2024-01-26 04:25:52.199371-07:00
description: "TOML\uFF0C\u4EE3\u8868Tom's Obvious, Minimal Language\uFF08\u6C64\u59C6\
  \u7684\u660E\u663E\u3001\u6700\u7B80\u8BED\u8A00\uFF09\uFF0C\u662F\u4E00\u79CD\u7C7B\
  \u4F3C\u4E8EJSON\u6216YAML\u7684\u6570\u636E\u5E8F\u5217\u5316\u683C\u5F0F\uFF0C\
  \u4F46\u5B83\u65E8\u5728\u7B80\u5355\u548C\u6613\u8BFB\u3002\u7A0B\u5E8F\u5458\u4F7F\
  \u7528TOML\u8FDB\u884C\u914D\u7F6E\u6587\u4EF6\u7F16\u5199\uFF0C\u56E0\u4E3A\u5B83\
  \u6613\u4E8E\u7F16\u5199\u548C\u7406\u89E3\uFF0C\u5E76\u4E14\u80FD\u591F\u6574\u9F50\
  \u5730\u6620\u5C04\u5230\u50CFPython\u8FD9\u6837\u7684\u7F16\u7A0B\u8BED\u8A00\u4E2D\
  \u7684\u6570\u636E\u7ED3\u6784\u4E0A\u3002"
lastmod: '2024-03-13T22:44:47.282559-06:00'
model: gpt-4-0125-preview
summary: "TOML\uFF0C\u4EE3\u8868Tom's Obvious, Minimal Language\uFF08\u6C64\u59C6\u7684\
  \u660E\u663E\u3001\u6700\u7B80\u8BED\u8A00\uFF09\uFF0C\u662F\u4E00\u79CD\u7C7B\u4F3C\
  \u4E8EJSON\u6216YAML\u7684\u6570\u636E\u5E8F\u5217\u5316\u683C\u5F0F\uFF0C\u4F46\
  \u5B83\u65E8\u5728\u7B80\u5355\u548C\u6613\u8BFB\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\
  TOML\u8FDB\u884C\u914D\u7F6E\u6587\u4EF6\u7F16\u5199\uFF0C\u56E0\u4E3A\u5B83\u6613\
  \u4E8E\u7F16\u5199\u548C\u7406\u89E3\uFF0C\u5E76\u4E14\u80FD\u591F\u6574\u9F50\u5730\
  \u6620\u5C04\u5230\u50CFPython\u8FD9\u6837\u7684\u7F16\u7A0B\u8BED\u8A00\u4E2D\u7684\
  \u6570\u636E\u7ED3\u6784\u4E0A\u3002."
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
