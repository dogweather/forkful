---
title:                "处理JSON数据"
html_title:           "Arduino: 处理JSON数据"
simple_title:         "处理JSON数据"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
JSON（JavaScript Object Notation）是一种在各种编程语言中常用的数据交换格式。Rust程序员用它来序列化数据，方便存储和网络传输。

## How to: (如何操作：)
在Rust中，常用`serde_json`库处理JSON。

```Rust
// 引入`serde_json`
use serde_json::{Value, json};

fn main() {
    // 创建一个JSON对象
    let person = json!({
        "name": "小明",
        "age": 25,
        "languages": ["Chinese", "English"]
    });

    // 序列化JSON对象为字符串
    let serialized = serde_json::to_string(&person).unwrap();
    println!("序列化: {}", serialized);

    // 反序列化字符串为JSON对象
    let deserialized: Value = serde_json::from_str(&serialized).unwrap();
    println!("反序列化: {:?}", deserialized);
}
```
运行代码，输出：
```
序列化: {"name":"小明","age":25,"languages":["Chinese","English"]}
反序列化: {"age":25,"languages":["Chinese","English"],"name":"小明"}
```

## Deep Dive (深入探讨：)
JSON在2000年代初期开始流行，取代了XML作为主要的数据交换格式。相比于XML，JSON更轻量，易于人阅读和编写，机器也容易解析和生成。Rust语言通过`serde`生态系统提供强大的序列化支持。除了`serde_json`之外，还有`serde_yaml`, `serde_xml`等库进行类似的序列化操作。

## See Also (另请参阅：)
- Serde官网: [https://serde.rs/](https://serde.rs/)
- Serde JSON 官网: [https://docs.serde.rs/serde_json/](https://docs.serde.rs/serde_json/)
- JSON官网: [https://www.json.org/json-zh.html](https://www.json.org/json-zh.html)