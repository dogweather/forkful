---
title:                "处理JSON数据"
date:                  2024-01-19
simple_title:         "处理JSON数据"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
在JavaScript中，JSON（JavaScript Object Notation）主要用于数据交换。因其易读性和网络传输的高效，它成了前后端通信中的黄金标准。

## 如何：
```javascript
// JSON 字符串解析成 JavaScript 对象
let jsonStr = '{"name":"张三", "age":30}';
let obj = JSON.parse(jsonStr);
console.log(obj.name); // 输出: 张三

// JavaScript 对象转换成 JSON 字符串
let jsonObj = {name: "李四", age: 40};
let str = JSON.stringify(jsonObj);
console.log(str); // 输出: {"name":"李四","age":40}
```

## 深入探索
JSON是在2000年代初由Douglas Crockford提出的，旨在替代繁琐的XML作为数据交换格式。尽管有其他格式如YAML、XML、protobuf等，JSON凭借它的简洁和JavaScript内置支持成为首选。直接通过`JSON.parse`和`JSON.stringify`，我们有最基本的解析和序列化机制，也可以处理错误、缩进或其他复杂场景。

## 参阅
- MDN关于JSON的文档：[MDN JSON](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/JSON)
- JSON 官方网站：[JSON.org](https://www.json.org/json-zh.html)
- 关于JSON的深入介绍文章：[Understanding JSON](https://www.digitalocean.com/community/tutorials/an-introduction-to-json)
