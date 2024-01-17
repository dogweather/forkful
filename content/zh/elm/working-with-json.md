---
title:                "与JSON一起工作"
html_title:           "Elm: 与JSON一起工作"
simple_title:         "与JSON一起工作"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/working-with-json.md"
---

{{< edit_this_page >}}

## 什么和为什么？

使用JSON是指将数据作为结构化文本传输的一种方式。程序员经常使用JSON来允许不同语言或平台之间相互通信和交换数据。

## 如何：

Elm使用Elm-JSON包来处理JSON数据。以下是一个简单的示例，展示如何将JavaScript对象转换为JSON字符串：

```
Elm.object [ ("name", "John"), ("age", 25), ("height", 180) ]
```

输出将是以下JSON字符串：

```
{ "name": "John", "age": 25, "height": 180 }
```

要从JSON字符串中解析数据，使用Elm-JSON包中的Decode模块。以下是一个示例，展示了如何从JSON字符串中解析姓名：

```
Elm.decodeString Elm.string "{ \"name\": \"John\" }"
```

输出将是字符串“John”。

## 深入探讨：

JSON（JavaScript对象表示）是一种轻量级的数据交换格式，起源于JavaScript编程语言。它已成为Web开发中的标准数据格式，也被广泛用于移动应用程序和API的数据传输。除了JSON之外，还有XML和CSV等其他数据格式，但JSON已经取得了主流地位。

Elm-JSON包是Elm编程语言用于处理JSON数据的主要包。它提供了一套简单的API来处理JSON数据，并且具有良好的性能和类型安全性。

除了Elm-JSON包之外，也可以在Elm中使用外部JavaScript库来处理JSON数据。但是使用Elm-JSON包可以避免使用JavaScript的不安全性和弱类型的问题。

## 参考链接：

- [Elm-JSON package](https://package.elm-lang.org/packages/elm/json/latest/)
- [JSON.org](https://www.json.org/json-en.html)
- [Elm programming language](https://elm-lang.org/)