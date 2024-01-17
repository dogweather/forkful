---
title:                "使用json进行编程"
html_title:           "C: 使用json进行编程"
simple_title:         "使用json进行编程"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/working-with-json.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

JSON是一种用于存储和交换数据的格式。程序员经常使用JSON来访问和处理数据，因为它简单、易于阅读和编写，并且被许多编程语言支持。

## 如何：

```C
#include <stdio.h>
#include <json-c/json.h>

int main() {
  // 创建一个JSON对象
  struct json_object *jobj = json_object_new_object();
  // 添加一个键值对
  json_object_object_add(jobj, "name", json_object_new_string("John"));
  // 将JSON对象转换为字符串并打印
  printf("JSON对象：%s\n", json_object_to_json_string(jobj));

  return 0;
}
```

输出：
```C
JSON对象：{"name": "John"}
```

## 深入探讨：

JSON最初是由JavaScript程序员创建的，用于取代XML作为数据交换的标准。除了C以外，许多编程语言都有针对JSON的官方或第三方库，例如Python中的json模块和Java中的GSON。程序员也可以使用纯C语言来解析和构建JSON，但使用json-c库可以更方便地处理。一些替代格式如XML和YAML也可以用来存储和交换数据，但它们通常比JSON更复杂和冗长。

## 参考资料：

- [JSON官方网站](https://www.json.org/)
- [json-c库的GitHub页面](https://github.com/json-c/json-c)