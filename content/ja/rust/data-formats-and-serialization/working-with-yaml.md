---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:04.022560-07:00
description: "\u65B9\u6CD5\uFF1A Rust\u306F\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\
  \u3067YAML\u3092\u30B5\u30DD\u30FC\u30C8\u3057\u3066\u3044\u306A\u3044\u305F\u3081\
  \u3001`serde`\uFF08\u30C7\u30FC\u30BF\u306E\u76F4\u5217\u5316\u304A\u3088\u3073\u9006\
  \u76F4\u5217\u5316\u306E\u305F\u3081\uFF09\u306A\u3069\u306E\u30B5\u30FC\u30C9\u30D1\
  \u30FC\u30C6\u30A3\u88FD\u306E\u30AF\u30EC\u30FC\u30C8\u3092\u4E00\u822C\u7684\u306B\
  \u4F7F\u7528\u3057\u307E\u3059\u3002\u3053\u308C\u306F`serde_yaml`\u3068\u7D44\u307F\
  \u5408\u308F\u305B\u3066\u4F7F\u7528\u3057\u307E\u3059\u3002 \u307E\u305A\u3001\
  `Cargo.toml`\u306B\u4F9D\u5B58\u95A2\u4FC2\u3092\u8FFD\u52A0\u3057\u307E\u3059\uFF1A\
  ."
lastmod: '2024-04-05T21:53:42.749538-06:00'
model: gpt-4-0125-preview
summary: "\u307E\u305A\u3001`Cargo.toml`\u306B\u4F9D\u5B58\u95A2\u4FC2\u3092\u8FFD\
  \u52A0\u3057\u307E\u3059\uFF1A."
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
weight: 41
---

## 方法：
Rustは標準ライブラリでYAMLをサポートしていないため、`serde`（データの直列化および逆直列化のため）などのサードパーティ製のクレートを一般的に使用します。これは`serde_yaml`と組み合わせて使用します。

まず、`Cargo.toml`に依存関係を追加します：

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_yaml = "0.8"
```

これから、YAML文字列をRust構造体に逆直列化する方法と、Rust構造体をYAML文字列に直列化する方法を見ていきます。

### YAMLをRust構造体に逆直列化する
YAMLで期待するデータを反映するRust構造体を定義します。カスタマイズが必要な場合はSerdeアトリビュートを使用します。

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

上記のRustコードを実行すると出力されるサンプル出力：

```plaintext
Config { name: "Shield", durability: 300, owner: Owner { name: "Steve", age: 25 } }
```

### Rust構造体をYAMLに直列化する
この例では、前のセクションの`Config`構造体を取り、それをYAML形式に戻して直列化します。

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

期待される出力はYAML形式の文字列になります：

```yaml
---
name: Axe
durability: 120
owner:
  name: Alex
  age: 30
```

これらのスニペットは、人気のある`serde`および`serde_yaml`クレートを使用して、複雑なデータ構造に対応し、単純で人間が読みやすい構成を提供することで、RustアプリケーションにYAMLの解析と生成を効率的に統合する方法を示しています。
