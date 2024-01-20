---
title:                "处理 YAML 文件"
html_title:           "Bash: 处理 YAML 文件"
simple_title:         "处理 YAML 文件"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?
YAML 是一种数据序列化格式，常用于配置文件和数据交换。程序员使用 YAML 的原因是它易读易写，同时也便于人类和计算机的解析。

## 如何操作：
让我们来看看如何在 Rust 中处理 YAML。首先，需要在 `Cargo.toml` 文件中添加 `serde` 和 `serde_yaml` 库：

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_yaml = "0.8"
```

现在，可以写代码解析 YAML 了：

```rust
use serde::{Deserialize, Serialize};
use serde_yaml;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Config {
    title: String,
    owner: Owner,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Owner {
    name: String,
    dob: String,
}

fn main() -> Result<(), serde_yaml::Error> {
    let yaml = r#"
        title: Example YAML
        owner:
          name: Z Lin
          dob: 1998-09-04
    "#;

    let deserialized_config: Config = serde_yaml::from_str(yaml)?;
    println!("{:?}", deserialized_config);

    Ok(())
}
```

代码中 `Config` 和 `Owner` 结构体定义了将要解析的 YAML 数据格式。程序运行后会输出：

```
Config { title: "Example YAML", owner: Owner { name: "Z Lin", dob: "1998-09-04" } }
```

## 深入了解：
YAML（YAML Ain't Markup Language）诞生于2001年，设计目标是易于人类阅读和编辑，同时也容易被机器解析。相比于JSON和XML，YAML更加强调可读性。在 Rust 中，通过 `serde` 库，我们可以轻松地序列化和反序列化数据。虽然 `serde` 支持多种格式，但处理 YAML 时要使用 `serde_yaml`。选择 YAML 而非 JSON 或 XML，常因为其更简洁明了的层次结构。

## 参考链接：
- Serde 官方文档：https://serde.rs
- Serde YAML 库文档：https://docs.rs/serde_yaml
- YAML 官方网站：https://yaml.org