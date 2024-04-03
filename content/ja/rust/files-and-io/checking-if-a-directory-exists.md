---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:46.832279-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.843710-06:00'
model: gpt-4-0125-preview
summary: "\u30BD\u30D5\u30C8\u30A6\u30A7\u30A2\u958B\u767A\u3067\u306F\u3001\u30D5\
  \u30A1\u30A4\u30EB\u306B\u30A2\u30AF\u30BB\u30B9\u3001\u8AAD\u307F\u51FA\u3057\u3001\
  \u66F8\u304D\u8FBC\u307F\u3092\u8A66\u307F\u308B\u969B\u306B\u30A8\u30E9\u30FC\u3092\
  \u907F\u3051\u308B\u305F\u3081\u306B\u3001\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\
  \u5B58\u5728\u3059\u308B\u304B\u3069\u3046\u304B\u3092\u78BA\u8A8D\u3059\u308B\u3053\
  \u3068\u304C\u3057\u3070\u3057\u3070\u5FC5\u8981\u3067\u3059\u3002\u30B7\u30B9\u30C6\
  \u30E0\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u8A00\u8A9E\u3067\u3042\u308BRust\u306F\
  \u3001\u3053\u306E\u30BF\u30B9\u30AF\u3092\u5B9F\u884C\u3059\u308B\u305F\u3081\u306E\
  \u5805\u7262\u306A\u65B9\u6CD5\u3092\u63D0\u4F9B\u3057\u3001\u30D7\u30ED\u30B0\u30E9\
  \u30E0\u304C\u30D5\u30A1\u30A4\u30EB\u3084\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u3092\
  \u5B89\u5168\u304B\u3064\u52B9\u7387\u7684\u306B\u6271\u3046\u3053\u3068\u304C\u3067\
  \u304D\u308B\u3088\u3046\u306B\u3057\u307E\u3059\u3002."
title: "\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\u3069\
  \u3046\u304B\u306E\u78BA\u8A8D"
weight: 20
---

## 方法：
Rustの標準ライブラリ（`std`）には、`std::path::Path` と `std::fs` モジュールを通じてディレクトリの存在を確認する機能が含まれています。以下は、Rustの標準的なアプローチを使用した簡単な例です：

```rust
use std::path::Path;

fn main() {
    let path = Path::new("/path/to/directory");
    if path.exists() && path.is_dir() {
        println!("ディレクトリは存在します。");
    } else {
        println!("ディレクトリは存在しません。");
    }
}
```

ディレクトリが存在すると仮定した場合のサンプル出力：
```
ディレクトリは存在します。
```

複雑なシナリオや拡張機能（非同期ファイルシステム操作など）に対処する場合は、非同期ランタイム内で作業している特に場合は、`tokio`のようなサードパーティのライブラリを検討するとよいでしょう。非同期にディレクトリが存在するかどうかを確認する方法は次のとおりです：

まず、`Cargo.toml`に`tokio`を追加します：

```toml
[dependencies]
tokio = { version = "1.0", features = ["full"] }
```

次に、`tokio::fs`を使って非同期にディレクトリが存在するか確認します：

```rust
use tokio::fs;

#[tokio::main]
async fn main() {
    let path = "/path/to/directory";
    match fs::metadata(path).await {
        Ok(metadata) => {
            if metadata.is_dir() {
                println!("ディレクトリは存在します。");
            } else {
                println!("パスは存在しますが、ディレクトリではありません。");
            }
        },
        Err(_) => println!("ディレクトリは存在しません。"),
    }
}
```

ディレクトリが存在しないと仮定した場合のサンプル出力：
```
ディレクトリは存在しません。
```

これらの例は、ソフトウェア開発の幅広いニーズに対応するために、Rustとそのエコシステムが同期および非同期のアプローチを提供していることを強調しています。
