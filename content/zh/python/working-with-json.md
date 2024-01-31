---
title:                "处理JSON数据"
date:                  2024-01-19
simple_title:         "处理JSON数据"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
什么是JSON？JSON是JavaScript Object Notation的缩写，用于存储和传输数据。为何使用JSON？因为它轻便、易读且易于和JavaScript等编程语言交互。

## How to:
```Python
import json

# JSON 字符串
json_string = '{"name": "小明", "age": 25, "city": "北京"}'

# 从字符串解析JSON
person = json.loads(json_string)
print(person)
# 输出：{'name': '小明', 'age': 25, 'city': '北京'}

# 从Python对象转换为JSON字符串
person_string = json.dumps(person, indent=4, ensure_ascii=False)
print(person_string)
# 输出：
# {
#     "name": "小明",
#     "age": 25,
#     "city": "北京"
# }
```

## Deep Dive
JSON起源于JavaScript, 但现成为语言独立的数据格式，广泛支持于许多编程语言。尽管XML曾是主流的数据交换格式，但由于JSON更为高效且易用，它已逐渐取代XML。在Python中，`json`模块提供了处理JSON数据的全面功能，包括解析（parsing）、生成（generating）、排序（sorting）等。

## See Also
- Python `json`模块官方文档: [https://docs.python.org/3/library/json.html](https://docs.python.org/3/library/json.html)
- JSON 规范介绍: [https://www.json.org/json-zh.html](https://www.json.org/json-zh.html)
- W3Schools JSON 教程: [https://www.w3schools.com/js/js_json_intro.asp](https://www.w3schools.com/js/js_json_intro.asp)
