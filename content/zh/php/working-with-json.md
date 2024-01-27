---
title:                "处理JSON数据"
date:                  2024-01-19
html_title:           "Arduino: 处理JSON数据"
simple_title:         "处理JSON数据"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? 什么以及为什么?
JSON, JavaScript Object Notation, 是数据交换的轻量级格式。程序员用它因为它简单易读，同时被大多数编程语言支持，包括PHP。

## How to: 怎么做
PHP处理JSON数据主要用两个函数：`json_encode()`和`json_decode()`。

```PHP
<?php
// 数组转JSON
$array = ['name' => '张三', 'age' => 28, 'city' => '北京'];
$json = json_encode($array);
echo $json;
// 输出: {"name":"张三","age":28,"city":"北京"}

// JSON转数组
$json = '{"name":"李四","age":35,"city":"上海"}';
$array = json_decode($json, true);
print_r($array);
// 输出: Array ( [name] => 李四 [age] => 35 [city] => 上海 )
?>
```

## Deep Dive 深入探究
JSON在2000s早期被发明，用作XML的轻量级替代品。除了XML，还有YAML和Protobuf等格式作为数据交换手段。在PHP中，`json_decode()`将JSON转为PHP数组或对象，而`json_encode()`将数组或对象转回JSON。需要注意的是错误处理，例如使用`json_last_error()`排查问题。

## See Also 另请参阅
- PHP官方JSON处理文档：[PHP: JSON - Manual](https://www.php.net/manual/en/book.json.php)
- JSON官方网站，了解格式细节：[JSON](https://www.json.org/json-en.html)
- 深入了解XML与JSON差异：[XML vs JSON](https://www.w3schools.com/js/js_json_xml.asp)
