---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:35:16.978404-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.846222-06:00'
model: gpt-4-0125-preview
summary: "Rust\u3067\u306E\u6A19\u6E96\u30A8\u30E9\u30FC\uFF08stderr\uFF09\u3078\u306E\
  \u66F8\u304D\u8FBC\u307F\u306F\u3001\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\
  \u3084\u8A3A\u65AD\u3092\u6A19\u6E96\u51FA\u529B\uFF08stdout\uFF09\u3068\u306F\u5225\
  \u306B\u30B3\u30F3\u30BD\u30FC\u30EB\u306B\u5411\u3051\u3066\u51FA\u529B\u3059\u308B\
  \u3053\u3068\u306B\u95A2\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u3053\u308C\u3092\u884C\u3046\u3053\u3068\u3067\u3001\u901A\u5E38\u306E\u30D7\
  \u30ED\u30B0\u30E9\u30E0\u51FA\u529B\u3068\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\
  \u30B8\u3092\u533A\u5225\u3057\u3001\u5B9F\u884C\u4E2D\u306B\u30A8\u30E9\u30FC\u3092\
  \u9069\u5207\u306B\u6271\u3063\u305F\u308A\u3001\u30ED\u30B0\u3084\u30D5\u30A1\u30A4\
  \u30EB\u306B\u30EA\u30C0\u30A4\u30EC\u30AF\u30C8\u3057\u305F\u308A\u3059\u308B\u306E\
  \u3092\u5BB9\u6613\u306B\u3057\u307E\u3059\u3002."
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
