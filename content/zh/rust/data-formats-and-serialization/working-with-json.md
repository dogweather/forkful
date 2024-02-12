---
title:                "使用JSON进行编程"
date:                  2024-02-03T19:24:11.320885-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用JSON进行编程"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 是什么 & 为什么？

在 Rust 中处理 JSON（JavaScript 对象表示法）涉及到将 JSON 数据解析为 Rust 数据结构，以及将 Rust 数据结构序列化回 JSON。程序员这样做是为了与网络 API、配置文件或任何使用 JSON 的数据交换格式进行交互，因为其轻量且易于人类阅读的格式。

## 如何操作：

在 Rust 中处理 JSON，广泛使用 `serde` 创建以及 `serde_json` 来进行序列化和反序列化。首先，确保在你的 `Cargo.toml` 中包含它们：

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
```

### 示例 1：将 JSON 反序列化为 Rust 结构体

定义一个 Rust 结构体，并使用 `Deserialize` 和 `Serialize` 的派生宏：

```rust
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
struct User {
    id: u32,
    name: String,
    email: String,
}

fn main() {
    let json_data = r#"
        {
            "id": 1,
            "name": "Jane Doe",
            "email": "jane.doe@example.com"
        }
    "#;

    let user: User = serde_json::from_str(json_data).unwrap();

    println!("用户 ID: {}", user.id);
    println!("用户名: {}", user.name);
    println!("用户邮箱: {}", user.email);
}
```

**输出：**

```
用户 ID: 1
用户名: Jane Doe
用户邮箱: jane.doe@example.com
```

### 示例 2：将 Rust 结构体序列化为 JSON

使用相同的 `User` 结构体：

```rust
let user = User {
    id: 1,
    name: "Jane Doe".to_string(),
    email: "jane.doe@example.com".to_string(),
};

let json_data = serde_json::to_string(&user).unwrap();

println!("{}", json_data);
```

**输出：**

```json
{"id":1,"name":"Jane Doe","email":"jane.doe@example.com"}
```

这些示例展示了将 JSON 反序列化为 Rust 结构体和将 Rust 结构体序列化回 JSON 字符串的基本流程。Serde 提供了一套丰富的 JSON 处理工具，包括处理可选字段、复杂嵌套和 JSON 不直接支持的类型。
