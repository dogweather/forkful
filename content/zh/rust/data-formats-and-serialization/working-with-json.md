---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:11.320885-07:00
description: "\u5728 Rust \u4E2D\u5904\u7406 JSON\uFF08JavaScript \u5BF9\u8C61\u8868\
  \u793A\u6CD5\uFF09\u6D89\u53CA\u5230\u5C06 JSON \u6570\u636E\u89E3\u6790\u4E3A Rust\
  \ \u6570\u636E\u7ED3\u6784\uFF0C\u4EE5\u53CA\u5C06 Rust \u6570\u636E\u7ED3\u6784\
  \u5E8F\u5217\u5316\u56DE JSON\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\
  \u4E86\u4E0E\u7F51\u7EDC API\u3001\u914D\u7F6E\u6587\u4EF6\u6216\u4EFB\u4F55\u4F7F\
  \u7528 JSON \u7684\u6570\u636E\u4EA4\u6362\u683C\u5F0F\u8FDB\u884C\u4EA4\u4E92\uFF0C\
  \u56E0\u4E3A\u5176\u8F7B\u91CF\u4E14\u6613\u4E8E\u4EBA\u7C7B\u9605\u8BFB\u7684\u683C\
  \u5F0F\u3002"
lastmod: '2024-03-13T22:44:47.545836-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Rust \u4E2D\u5904\u7406 JSON\uFF08JavaScript \u5BF9\u8C61\u8868\u793A\
  \u6CD5\uFF09\u6D89\u53CA\u5230\u5C06 JSON \u6570\u636E\u89E3\u6790\u4E3A Rust \u6570\
  \u636E\u7ED3\u6784\uFF0C\u4EE5\u53CA\u5C06 Rust \u6570\u636E\u7ED3\u6784\u5E8F\u5217\
  \u5316\u56DE JSON\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u4E0E\
  \u7F51\u7EDC API\u3001\u914D\u7F6E\u6587\u4EF6\u6216\u4EFB\u4F55\u4F7F\u7528 JSON\
  \ \u7684\u6570\u636E\u4EA4\u6362\u683C\u5F0F\u8FDB\u884C\u4EA4\u4E92\uFF0C\u56E0\
  \u4E3A\u5176\u8F7B\u91CF\u4E14\u6613\u4E8E\u4EBA\u7C7B\u9605\u8BFB\u7684\u683C\u5F0F\
  \u3002"
title: "\u4F7F\u7528JSON\u8FDB\u884C\u7F16\u7A0B"
weight: 38
---

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
