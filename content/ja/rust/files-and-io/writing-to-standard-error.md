---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:35:16.978404-07:00
description: "\u65B9\u6CD5\uFF1A Rust\u306F`eprintln!`\u30DE\u30AF\u30ED\u3092\u4F7F\
  \u7528\u3057\u3066stderr\u306B\u66F8\u304D\u8FBC\u3080\u76F4\u89B3\u7684\u306A\u65B9\
  \u6CD5\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002\u3053\u308C\u306F`println!`\u304C\
  stdout\u306B\u4F7F\u7528\u3055\u308C\u308B\u306E\u3068\u540C\u69D8\u3067\u3059\u3002\
  \u57FA\u672C\u7684\u306A\u4F8B\u3092\u4EE5\u4E0B\u306B\u793A\u3057\u307E\u3059\uFF1A\
  ."
lastmod: '2024-04-05T22:37:50.128271-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A Rust\u306F`eprintln!`\u30DE\u30AF\u30ED\u3092\u4F7F\u7528\
  \u3057\u3066stderr\u306B\u66F8\u304D\u8FBC\u3080\u76F4\u89B3\u7684\u306A\u65B9\u6CD5\
  \u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002\u3053\u308C\u306F`println!`\u304Cstdout\u306B\
  \u4F7F\u7528\u3055\u308C\u308B\u306E\u3068\u540C\u69D8\u3067\u3059\u3002\u57FA\u672C\
  \u7684\u306A\u4F8B\u3092\u4EE5\u4E0B\u306B\u793A\u3057\u307E\u3059\uFF1A."
title: "\u6A19\u6E96\u30A8\u30E9\u30FC\u3078\u306E\u66F8\u304D\u8FBC\u307F"
weight: 25
---

## 方法：
Rustは`eprintln!`マクロを使用してstderrに書き込む直観的な方法を提供します。これは`println!`がstdoutに使用されるのと同様です。基本的な例を以下に示します：

```rust
fn main() {
    eprintln!("これはエラーメッセージです！");
}
```

標準エラーへのサンプル出力：
```
これはエラーメッセージです！
```

エラーメッセージをよりコントロールしたい場合、例えばテキストをフォーマットしたり、I/Oの結果を扱いたりする場合は、`std::io`モジュールから`stderr`関数を使用します。この方法はグローバルなstderrストリームへのハンドルを提供し、`Write`トレイトからの`write_all`や`writeln`のようなメソッドを使用して書き込むことができます：

```rust
use std::io::{self, Write};

fn main() {
    let stderr = io::stderr();
    let mut handle = stderr.lock();
    
    writeln!(handle, "フォーマットされたエラーメッセージ：{}", 404).expect("stderrへの書き込みに失敗");
}
```

標準エラーへのサンプル出力：
```
フォーマットされたエラーメッセージ：404
```

ライブラリを頼りにする環境やアプリケーションで作業している場合、`log`や`env_logger`のようなライブラリが人気です。これらはログ記録の目的でより多用されていますが、設定可能でエラーログレベルをstderrに向けることができます。以下は`log`と`env_logger`を使用した簡単な使用例です：

まず、`Cargo.toml`に依存関係を追加します：
```toml
[dependencies]
log = "0.4"
env_logger = "0.9"
```

次に、アプリケーションでロギングを設定して使用します：
```rust
fn main() {
    env_logger::init();
    log::error!("これはstderrに記録されたエラーメッセージです");
}
```

このプログラムを実行する（適切な環境変数、例えば`RUST_LOG=error`で`env_logger`を設定した後）と、ログインフラストラクチャを利用してエラーメッセージがstderrに出力されます。

```plaintext
ERROR: これはstderrに記録されたエラーメッセージです
```
