---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:46.832279-07:00
description: "\u65B9\u6CD5\uFF1A Rust\u306E\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\
  \uFF08`std`\uFF09\u306B\u306F\u3001`std::path::Path` \u3068 `std::fs` \u30E2\u30B8\
  \u30E5\u30FC\u30EB\u3092\u901A\u3058\u3066\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u306E\
  \u5B58\u5728\u3092\u78BA\u8A8D\u3059\u308B\u6A5F\u80FD\u304C\u542B\u307E\u308C\u3066\
  \u3044\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u3001Rust\u306E\u6A19\u6E96\u7684\u306A\
  \u30A2\u30D7\u30ED\u30FC\u30C1\u3092\u4F7F\u7528\u3057\u305F\u7C21\u5358\u306A\u4F8B\
  \u3067\u3059\uFF1A."
lastmod: '2024-03-13T22:44:41.843710-06:00'
model: gpt-4-0125-preview
summary: "Rust\u306E\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\uFF08`std`\uFF09\u306B\
  \u306F\u3001`std::path::Path` \u3068 `std::fs` \u30E2\u30B8\u30E5\u30FC\u30EB\u3092\
  \u901A\u3058\u3066\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u306E\u5B58\u5728\u3092\u78BA\
  \u8A8D\u3059\u308B\u6A5F\u80FD\u304C\u542B\u307E\u308C\u3066\u3044\u307E\u3059\u3002\
  \u4EE5\u4E0B\u306F\u3001Rust\u306E\u6A19\u6E96\u7684\u306A\u30A2\u30D7\u30ED\u30FC\
  \u30C1\u3092\u4F7F\u7528\u3057\u305F\u7C21\u5358\u306A\u4F8B\u3067\u3059\uFF1A."
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
