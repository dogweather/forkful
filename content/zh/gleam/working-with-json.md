---
title:                "使用json进行编程"
html_title:           "Gleam: 使用json进行编程"
simple_title:         "使用json进行编程"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## 什么是JSON，为什么程序员需要它？

JSON（JavaScript Object Notation）是一种轻量级的数据交换格式，通常用于在网络应用程序中传输和存储数据。它是一种易于阅读和编写的格式，也易于计算机解析和生成。程序员经常使用JSON来存储和传输数据，特别是在Web开发中。

## 如何使用JSON?

Gleam提供了一个内置的JSON模块，可以方便地解析和生成JSON数据。下面是一个使用JSON模块的示例代码：

```Gleam
import gleam/json

let user = {
  "name": "John",
  "age": 30,
  "hobbies": ["coding", "reading", "hiking"]
}

let json = user
|> json.encode

// Output
{
  "name": "John",
  "age": 30,
  "hobbies": ["coding", "reading", "hiking"]
}
```

可以看到，我们首先导入了Gleam的JSON模块，然后定义了一个名为`user`的变量，它包含了一个名字、年龄和爱好的列表。接着，我们使用`json.encode`函数将该变量编码成JSON格式，并将结果赋值给`json`变量。最后，我们输出`json`变量的值，可以看到它包含了与`user`相同的数据，但格式为JSON。

如果我们想从JSON数据中读取信息，可以使用`json.decode`函数。下面是一个示例代码：

```Gleam
import gleam/json

let json = "{\"name\": \"John\", \"age\": 30, \"hobbies\": [\"coding\", \"reading\", \"hiking\"]}"

let user = json
|> json.decode

// Output
{
  "name": "John",
  "age": 30,
  "hobbies": ["coding", "reading", "hiking"]
}
```

同样的，我们导入JSON模块，然后定义了一个名为`json`的变量，它包含了一个JSON格式的字符串。然后，我们使用`json.decode`函数将该字符串解析成Gleam对象，并将结果赋值给`user`变量。最后，我们输出`user`变量的值，可以看到它与之前的`user`变量相同。

## 深入了解

JSON最初是由Douglas Crockford在2001年创建的，它是一种基于JavaScript语法的轻量级数据交换格式。JSON旨在替代XML和JavaScript中使用的对象字面量作为数据传输的方式。它已经成为了Web应用程序中最常用的数据交换格式之一。

除了Gleam的JSON模块之外，程序员还可以使用其他语言和框架来处理JSON数据。比如，JavaScript的`JSON.parse()`和`JSON.stringify()`方法可以用来解析和生成JSON数据。在Web开发中，也有许多其他框架和库可以处理JSON，如React和Vue。

## 查看更多

- [JSON官方网站](https://www.json.org/json-en.html)
- [JavaScript的JSON对象文档](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON)