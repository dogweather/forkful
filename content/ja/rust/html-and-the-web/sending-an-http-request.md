---
date: 2024-01-20 18:00:45.729167-07:00
description: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u3068\u306F\u3001\
  \u30B5\u30FC\u30D0\u306B\u30C7\u30FC\u30BF\u3092\u8981\u6C42\u3059\u308B\u3053\u3068\
  \u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u306E\u30D7\u30ED\
  \u30BB\u30B9\u3092\u901A\u3058\u3066\u3001\u30A6\u30A7\u30D6API\u304B\u3089\u60C5\
  \u5831\u3092\u53D6\u5F97\u3057\u305F\u308A\u3001\u30A6\u30A7\u30D6\u30B5\u30FC\u30D3\
  \u30B9\u3068\u901A\u4FE1\u3057\u305F\u308A\u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:00.993536
model: gpt-4-1106-preview
summary: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u3068\u306F\u3001\u30B5\
  \u30FC\u30D0\u306B\u30C7\u30FC\u30BF\u3092\u8981\u6C42\u3059\u308B\u3053\u3068\u3067\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u306E\u30D7\u30ED\u30BB\
  \u30B9\u3092\u901A\u3058\u3066\u3001\u30A6\u30A7\u30D6API\u304B\u3089\u60C5\u5831\
  \u3092\u53D6\u5F97\u3057\u305F\u308A\u3001\u30A6\u30A7\u30D6\u30B5\u30FC\u30D3\u30B9\
  \u3068\u901A\u4FE1\u3057\u305F\u308A\u3057\u307E\u3059\u3002"
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
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
