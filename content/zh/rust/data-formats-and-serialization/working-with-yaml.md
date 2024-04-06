---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:42.057132-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Rust \u5728\u5176\u6807\u51C6\u5E93\u4E2D\
  \u4E0D\u652F\u6301 YAML\uFF0C\u56E0\u6B64\u6211\u4EEC\u901A\u5E38\u4F7F\u7528\u7B2C\
  \u4E09\u65B9\u5E93\uFF0C\u5982 `serde`\uFF08\u7528\u4E8E\u5E8F\u5217\u5316\u548C\
  \u53CD\u5E8F\u5217\u5316\u6570\u636E\uFF09\u7ED3\u5408 `serde_yaml`\u3002 \u9996\
  \u5148\uFF0C\u5728\u4F60\u7684 `Cargo.toml` \u4E2D\u6DFB\u52A0\u4F9D\u8D56\uFF1A\
  ."
lastmod: '2024-04-05T21:53:47.863370-06:00'
model: gpt-4-0125-preview
summary: "\u9996\u5148\uFF0C\u5728\u4F60\u7684 `Cargo.toml` \u4E2D\u6DFB\u52A0\u4F9D\
  \u8D56\uFF1A."
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
weight: 41
---

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
