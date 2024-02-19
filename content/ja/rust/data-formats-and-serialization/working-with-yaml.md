---
aliases:
- /ja/rust/working-with-yaml/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:04.022560-07:00
description: "Rust\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306B\u304A\u3044\u3066\
  \u3001YAML\uFF08YAML Ain't Markup\u2026"
lastmod: 2024-02-18 23:08:54.739537
model: gpt-4-0125-preview
summary: "Rust\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306B\u304A\u3044\u3066\u3001\
  YAML\uFF08YAML Ain't Markup\u2026"
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
---

{{< edit_this_page >}}

## 何となぜ？

Rustプログラミングにおいて、YAML（YAML Ain't Markup Language）を扱うことは、人間に優しいデータ直列化標準であるYAML形式のデータを解析し生成することについてです。プログラマーは、アプリケーションの設定、設定の管理、または複雑なデータ構造を明瞭かつ読みやすい形式で処理するために、RustでYAML処理を統合します。これは、構成ファイルやデータ交換のためのJSONやXMLよりもその単純さを活用します。

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
