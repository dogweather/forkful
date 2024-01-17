---
title:                "json 编程技巧"
html_title:           "Python: json 编程技巧"
simple_title:         "json 编程技巧"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/working-with-json.md"
---

{{< edit_this_page >}}

## 什么是JSON & 为什么要用它？
JSON是一种从Web传输数据的格式，它简单易读，适合计算机解析。程序员使用JSON来处理数据，使其易于传输和储存。

## 如何处理JSON:
```Python
import json
# 创建一个JSON字符串
json_str = '{"name": "John", "age": 30, "city": "New York"}'
# 将JSON转换成Python字典
data = json.loads(json_str)
# 输出Python字典的键和值
print(data["name"]) # John
print(data["age"]) # 30
print(data["city"]) # New York
```

## 深入了解:
JSON是JavaScript Object Notation的缩写，起源于2001年。它是一种轻量级数据交换格式，可以更有效地传输大量数据。使用XML也可以达到相同的功能，但JSON更易于解析且更适合Web环境。

## 参考链接:
- [JSON官方网站](https://www.json.org/json-en.html)
- [Python官方文档-JSON模块](https://docs.python.org/3/library/json.html)