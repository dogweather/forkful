---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:12.467377-07:00
description: "Rust\u3067\u306EJSON\uFF08JavaScript Object\u2026"
lastmod: '2024-03-13T22:44:41.852043-06:00'
model: gpt-4-0125-preview
summary: "Rust\u3067\u306EJSON\uFF08JavaScript Object Notation\uFF09\u3092\u6271\u3046\
  \u3053\u3068\u306F\u3001JSON\u30C7\u30FC\u30BF\u3092Rust\u306E\u30C7\u30FC\u30BF\
  \u69CB\u9020\u306B\u30D1\u30FC\u30B9\u3059\u308B\u3053\u3068\u3068\u3001Rust\u306E\
  \u30C7\u30FC\u30BF\u69CB\u9020\u3092JSON\u306B\u30B7\u30EA\u30A2\u30E9\u30A4\u30BA\
  \u3059\u308B\u3053\u3068\u306B\u3064\u3044\u3066\u3067\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u3001\u8EFD\u91CF\u3067\u4EBA\u9593\u304C\u8AAD\u307F\u3084\
  \u3059\u3044\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3067\u3042\u308B\u305F\u3081\u3001\
  \u30A6\u30A7\u30D6API\u3001\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u3001\u307E\u305F\
  \u306FJSON\u304C\u4F7F\u7528\u3055\u308C\u308B\u4EFB\u610F\u306E\u30C7\u30FC\u30BF\
  \u4EA4\u63DB\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3068\u3084\u308A\u53D6\u308A\u3059\
  \u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002."
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
weight: 38
---

## 何となぜ？

RustでのJSON（JavaScript Object Notation）を扱うことは、JSONデータをRustのデータ構造にパースすることと、Rustのデータ構造をJSONにシリアライズすることについてです。プログラマーは、軽量で人間が読みやすいフォーマットであるため、ウェブAPI、設定ファイル、またはJSONが使用される任意のデータ交換フォーマットとやり取りするためにこれを行います。

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
