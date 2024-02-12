---
title:                "標準エラーへの書き込み"
aliases: - /ja/rust/writing-to-standard-error.md
date:                  2024-02-03T19:35:16.978404-07:00
model:                 gpt-4-0125-preview
simple_title:         "標準エラーへの書き込み"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
Rustでの標準エラー（stderr）への書き込みは、エラーメッセージや診断を標準出力（stdout）とは別にコンソールに向けて出力することに関します。プログラマーはこれを行うことで、通常のプログラム出力とエラーメッセージを区別し、実行中にエラーを適切に扱ったり、ログやファイルにリダイレクトしたりするのを容易にします。

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
