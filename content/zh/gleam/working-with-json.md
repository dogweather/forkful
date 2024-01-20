---
title:                "处理JSON数据"
html_title:           "Arduino: 处理JSON数据"
simple_title:         "处理JSON数据"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
什么 & 为什么?

JSON是一种轻量级数据交换格式。编程时，我们使用JSON来存储和传送结构化数据。好处是易于人阅读，而且被大多数编程语言支持。

## How to:
怎么做:

```Gleam
import gleam/json

// 定义一个Person类型
pub type Person {
  Person(name: String, age: Int)
}

// JSON解析成Gleam类型
pub fn decode_person(json_string: String) -> Result(Person, json.DecodeError) {
  json_string
  |> json.decode(.object(from_pairs))
  |> result.map(fn(pair) {
    match pair {
      {"name", name}, {"age", age} => Ok(Person(name, age))
      _ => Error(json.DecodeError)
    }
  })
}

// 转换Gleam类型成JSON
pub fn encode_person(person: Person) -> String {
  match person {
    Person(name, age) =>
      json.encode(.object([ 
        "name", json.string(name), 
        "age", json.int(age)
      ]))
  }
}
```
示例输出:
```json
{"name":"张三","age":30}
```

## Deep Dive
深入了解

JSON, 或 JavaScript Object Notation，起源于JavaScript。现在，它很独立，大部分语言都能处理。替代品有XML和YAML，但JSON结构更加简洁。Gleam中处理JSON需要显式定义数据结构，并处理可能出现的错误。

## See Also
另请参阅

- [The official JSON website](https://www.json.org/json-en.html)
- [Mozilla JSON guide](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/JSON)