---
title:                "与 json 编程"
html_title:           "PHP: 与 json 编程"
simple_title:         "与 json 编程"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/working-with-json.md"
---

{{< edit_this_page >}}

## 什么是 JSON 和为什么程序员需要它?

JSON（JavaScript Object Notation）是一种轻量级的数据交换格式，可用于存储和传输数据。它使用易于阅读和编写的文本格式，并且易于解析和生成，适用于各种编程语言。程序员经常使用JSON来存储和传输数据，这使得数据交换更加方便和高效。

## 如何使用:

#### 创建一个 JSON 对象:
```
$person = [
  "name" => "John",
  "age" => 32,
  "hobbies" => ["coding", "reading", "gaming"]
];
```

#### 将 JSON 对象转换为字符串:
```
$json = json_encode($person);
```

#### 将 JSON 字符串转换为对象:
```
$person = json_decode($json);
```

## 深入了解:

- JSON最初由Douglas Crockford在20世纪90年代提出，旨在取代XML作为数据交换的首选格式。
- 除了JSON，还有其他格式如XML和YAML可用于存储和传输数据，但JSON具有易于阅读和编写的优点。
- 在PHP中，使用`json_encode()`函数将PHP数组转换为JSON字符串，使用`json_decode()`函数将JSON字符串转换为PHP对象或数组。

## 查看更多:

- PHP官方文档：https://www.php.net/manual/en/book.json.php
- JSON官方网站：https://www.json.org/
- 高性能JSON库：https://github.com/salsify/jsonstreamingparser