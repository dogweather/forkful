---
title:                "HTTPリクエストの送信"
aliases:
- ja/rust/sending-an-http-request.md
date:                  2024-01-20T18:00:45.729167-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTPリクエストの送信"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
HTTPリクエストを送るとは、サーバにデータを要求することです。プログラマーはこのプロセスを通じて、ウェブAPIから情報を取得したり、ウェブサービスと通信したりします。

## How to: (やり方)
RustでHTTPリクエストを送る基本的なコードを見てみましょう。`reqwest`クレートを使うと簡単です。Cargo.tomlに依存を追加してください。

```toml
[dependencies]
reqwest = "0.11"
tokio = { version = "1", features = ["full"] }
```

さあ、コードを書きましょう。

```rust
#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let res = reqwest::get("https://httpbin.org/ip").await?.text().await?;
    println!("Response: {}", res);
    Ok(())
}
```

実行すると、こんな出力が見られます。

```
Response: {
  "origin": "Your IP address"
}
```

## Deep Dive (深掘り)
HTTPリクエストの送信は、インターネットの基盤です。最初はcURLなどのツールで行われていましたが、今ではほとんどのプログラミング言語でサポートされています。

`reqwest`はRustで一番人気のあるHTTPクライアントライブラリですが、代わりに`hyper`を低レベルで使うこともできますし、`curl`バインディングを使うこともできます。

実装の詳細については、`reqwest`は非同期I/O（`async/await`）をサポートし、TLSを扱うための`rustls`を使っています。これにより、安全で効率的なHTTPリクエストが可能となります。

## See Also (関連情報)
- 公式`reqwest`ドキュメント: https://docs.rs/reqwest/
- `async/await`の説明: https://rust-lang.github.io/async-book/
- `tokio`ランタイムの案内: https://tokio.rs/

コードを書く時にこれらのリソースを参考にしてみてください。
