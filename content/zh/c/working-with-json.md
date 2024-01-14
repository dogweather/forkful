---
title:                "C: 与 json 的程序设计"
simple_title:         "与 json 的程序设计"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/working-with-json.md"
---

{{< edit_this_page >}}

## 为什么会选择使用JSON

有很多不同的编程语言和数据格式可供选择，那么为什么我们应该选择使用JSON呢？首先，它具有简单易懂的语法，适合用于存储和传输数据。同时，它也是一种轻量级的格式，能够节省网络带宽和存储空间。因此，许多编程语言都提供了支持JSON的库和工具，使它成为一种流行的数据格式。

## 如何使用JSON进行编程

下面我们将介绍如何在C语言中使用JSON，包括创建、解析和修改JSON数据。

首先，需要引入JSON库，比如[cJSON](https://github.com/DaveGamble/cJSON)。然后，我们可以使用`cJSON_CreateObject()`来创建一个JSON对象：

```C
cJSON *root = cJSON_CreateObject();
```

接着，我们可以向JSON对象中添加键值对：

```C
cJSON_AddStringToObject(root, "name", "John");
cJSON_AddNumberToObject(root, "age", 25);
```

我们也可以添加嵌套的JSON对象或数组：

```C
cJSON *address = cJSON_CreateObject();
cJSON_AddStringToObject(address, "street", "Main Street");
cJSON_AddStringToObject(address, "city", "New York");
cJSON_AddItemToObject(root, "address", address);

cJSON *hobbies = cJSON_CreateArray();
cJSON_AddItemToObject(hobbies, "reading");
cJSON_AddItemToObject(hobbies, "hiking");
cJSON_AddItemToObject(root, "hobbies", hobbies);
```

最后，我们可以将JSON对象转换为字符串并打印出来：

```C
char *json_str = cJSON_Print(root);
printf("%s\n", json_str);
```

输出结果为：

```
{
  "name": "John",
  "age": 25,
  "address": {
    "street": "Main Street",
    "city": "New York"
  },
  "hobbies": [
    "reading",
    "hiking"
  ]
}
```

## 深入了解JSON

JSON还有许多其他的特性，比如可以包含空值、布尔值和特殊字符。它也可以用来表示复杂的数据结构，比如树和图。此外，JSON还有一些高级的功能，比如处理不同类型的数据和自定义解析规则。

在使用JSON时，也需要注意一些安全性问题，比如防范恶意的JSON数据，避免发生内存泄漏等。

## 另请参阅

- [JSON官方网站](https://www.json.org/)
- [cJSON库的使用指南](https://github.com/DaveGamble/cJSON/blob/master/README.md)
- [JSON解析器的性能对比](https://github.com/msoap/shmbench/wiki/Tests-for-C-library-for-working-with-JSON-data)