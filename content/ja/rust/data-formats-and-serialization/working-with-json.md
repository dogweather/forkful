---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:12.467377-07:00
description: "\u3069\u3046\u3084\u3063\u3066\uFF1A Rust\u3067JSON\u3092\u6271\u3046\
  \u305F\u3081\u306B\u3001\u30B7\u30EA\u30A2\u30E9\u30A4\u30BC\u30FC\u30B7\u30E7\u30F3\
  \u3068\u30C7\u30B7\u30EA\u30A2\u30E9\u30A4\u30BC\u30FC\u30B7\u30E7\u30F3\u306E\u305F\
  \u3081\u306B`serde`\u30AF\u30EC\u30FC\u30C8\u3068`serde_json`\u304C\u5E83\u304F\u4F7F\
  \u7528\u3055\u308C\u307E\u3059\u3002\u307E\u305A\u3001\u3053\u308C\u3089\u3092\u3042\
  \u306A\u305F\u306E`Cargo.toml`\u306B\u542B\u3081\u3066\u3044\u308B\u3053\u3068\u3092\
  \u78BA\u8A8D\u3057\u3066\u304F\u3060\u3055\u3044\uFF1A."
lastmod: '2024-03-13T22:44:41.852043-06:00'
model: gpt-4-0125-preview
summary: "Rust\u3067JSON\u3092\u6271\u3046\u305F\u3081\u306B\u3001\u30B7\u30EA\u30A2\
  \u30E9\u30A4\u30BC\u30FC\u30B7\u30E7\u30F3\u3068\u30C7\u30B7\u30EA\u30A2\u30E9\u30A4\
  \u30BC\u30FC\u30B7\u30E7\u30F3\u306E\u305F\u3081\u306B`serde`\u30AF\u30EC\u30FC\u30C8\
  \u3068`serde_json`\u304C\u5E83\u304F\u4F7F\u7528\u3055\u308C\u307E\u3059\u3002\u307E\
  \u305A\u3001\u3053\u308C\u3089\u3092\u3042\u306A\u305F\u306E`Cargo.toml`\u306B\u542B\
  \u3081\u3066\u3044\u308B\u3053\u3068\u3092\u78BA\u8A8D\u3057\u3066\u304F\u3060\u3055\
  \u3044\uFF1A."
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
weight: 38
---

## どうやって：
RustでJSONを扱うために、シリアライゼーションとデシリアライゼーションのために`serde`クレートと`serde_json`が広く使用されます。まず、これらをあなたの`Cargo.toml`に含めていることを確認してください：

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
```

### 例1: JSONをRust構造体にデシリアライズする
Rust構造体を定義し、`Deserialize`と`Serialize`のためのderiveマクロを使用します：

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

    println!("ユーザーID: {}", user.id);
    println!("ユーザー名: {}", user.name);
    println!("ユーザーのメール: {}", user.email);
}
```

**出力:**

```
ユーザーID: 1
ユーザー名: Jane Doe
ユーザーのメール: jane.doe@example.com
```

### 例2: Rust構造体をJSONにシリアライズする
同じ`User`構造体を使用して：

```rust
let user = User {
    id: 1,
    name: "Jane Doe".to_string(),
    email: "jane.doe@example.com".to_string(),
};

let json_data = serde_json::to_string(&user).unwrap();

println!("{}", json_data);
```

**出力:**

```json
{"id":1,"name":"Jane Doe","email":"jane.doe@example.com"}
```

これらの例は、JSONをRust構造体にデシリアライズし、Rust構造体をJSON文字列にシリアライズする基本的な流れを示しています。Serdeは、オプショナルフィールド、複雑なネスティング、JSONに直接サポートされていないタイプなど、JSONを扱うための豊富なツールセットを提供します。
