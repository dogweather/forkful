---
title:                "使用YAML工作"
aliases:
- /zh/rust/working-with-yaml/
date:                  2024-02-03T19:26:42.057132-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用YAML工作"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

在 Rust 编程中，处理 YAML（YAML 不是标记语言）涉及到在 YAML 格式中解析和生成数据，这是一种对人类友好的数据序列化标准。程序员在 Rust 中集成 YAML 处理以配置应用程序、管理设置或以清晰、可读的格式处理复杂的数据结构，利用其相对于 JSON 或 XML 在配置文件和数据交换中的简单性。

## 如何操作：

Rust 在其标准库中不支持 YAML，因此我们通常使用第三方库，如 `serde`（用于序列化和反序列化数据）结合 `serde_yaml`。

首先，在你的 `Cargo.toml` 中添加依赖：

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_yaml = "0.8"
```

现在，让我们看看如何将 YAML 字符串反序列化成 Rust 结构体，以及如何将 Rust 结构体序列化回 YAML 字符串。

### 将 YAML 反序列化为 Rust 结构

定义一个 Rust 结构体以反映你在 YAML 中期望的数据。如果需要，可以使用 Serde 属性进行定制。

```rust
use serde::{Deserialize, Serialize};
use serde_yaml;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Config {
    name: String,
    durability: i32,
    owner: Owner,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Owner {
    name: String,
    age: i32,
}

fn main() {
    let yaml_data = "
name: Shield
durability: 300
owner:
  name: Steve
  age: 25
";

    let deserialized_config: Config = serde_yaml::from_str(yaml_data).unwrap();
    println!("{:?}", deserialized_config);
}
```

运行上述 Rust 代码后的示例输出将为：

```plaintext
Config { name: "Shield", durability: 300, owner: Owner { name: "Steve", age: 25 } }
```

### 将 Rust 结构序列化为 YAML

此示例采用前一节中的 `Config` 结构体，并将其序列化回 YAML 格式。

```rust
fn main() {
    let config = Config {
        name: String::from("Axe"),
        durability: 120,
        owner: Owner {
            name: String::from("Alex"),
            age: 30,
        },
    };

    let serialized_yaml = serde_yaml::to_string(&config).unwrap();
    println!("{}", serialized_yaml);
}
```

预期输出将是一个 YAML 格式的字符串：

```yaml
---
name: Axe
durability: 120
owner:
  name: Alex
  age: 30
```

这些代码片段演示了如何在你的 Rust 应用程序中有效地集成 YAML 解析和生成，使用流行的 `serde` 和 `serde_yaml` 库，适应复杂的数据结构并提供简单、易读的配置。
