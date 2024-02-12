---
title:                "JSONを活用する"
aliases:
- /ja/rust/working-with-json.md
date:                  2024-02-03T19:24:12.467377-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSONを活用する"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
