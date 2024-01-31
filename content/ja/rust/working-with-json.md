---
title:                "JSONを扱う方法"
date:                  2024-01-19
html_title:           "Arduino: JSONを扱う方法"
simple_title:         "JSONを扱う方法"

category:             "Rust"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? / 何となぜ？

JSONはデータ交換のフォーマットです。RustでJSONを扱うことで、Web APIや設定ファイルなどとデータを簡単にやり取りできます。

## How to: / どのようにして：

以下に、RustでJSONを扱う基本的な方法を示します。

```Rust
// serde_json dependency is needed in Cargo.toml

use serde::{Deserialize, Serialize};
use serde_json::{Result, Value};

#[derive(Serialize, Deserialize)]
struct Person {
    name: String,
    age: u8,
    is_programmer: bool,
}

fn main() -> Result<()> {
    // Serialize
    let person = Person {
        name: "Alice".to_string(),
        age: 30,
        is_programmer: true,
    };
    let serialized = serde_json::to_string(&person)?;
    println!("Serialized: {}", serialized);

    // Deserialize
    let deserialized: Person = serde_json::from_str(&serialized)?;
    println!("Deserialized: {} is {} years old.", deserialized.name, deserialized.age);

    // Parse arbitrary JSON
    let data = r#"
        {
            "name": "Bob",
            "age": null,
            "is_programmer": false
        }"#;
    let v: Value = serde_json::from_str(data)?;
    println!("Parsed name: {}", v["name"]);

    Ok(())
}
```

Sample output:

```
Serialized: {"name":"Alice","age":30,"is_programmer":true}
Deserialized: Alice is 30 years old.
Parsed name: "Bob"
```

## Deep Dive / ディープダイブ

JSONはJavaScript Object Notationの略で、2001年に導入されました。Rustでは`serde_json`クレートと`serde`ライブラリでJSONを扱います。`serde`はシリアライズとデシリアライズのためのフレームワークです。XMLやYAMLなどの他のフォーマットもありますが、JSONはその軽量さと人間が読める形式で広く使われています。

## See Also / 参照

- [`serde_json` documentation](https://docs.serde.rs/serde_json/)
- [The Rust Programming Language – Working with JSON](https://doc.rust-lang.org/book/ch20-00-final-project-a-web-server.html#storing-random-numbers-associated-with-an-id-in-the-hash-map)
- [`serde` crate documentation](https://serde.rs/)
