---
date: 2024-01-20 18:00:45.729167-07:00
description: "How to: (\u3084\u308A\u65B9) Rust\u3067HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\
  \u3092\u9001\u308B\u57FA\u672C\u7684\u306A\u30B3\u30FC\u30C9\u3092\u898B\u3066\u307F\
  \u307E\u3057\u3087\u3046\u3002`reqwest`\u30AF\u30EC\u30FC\u30C8\u3092\u4F7F\u3046\
  \u3068\u7C21\u5358\u3067\u3059\u3002Cargo.toml\u306B\u4F9D\u5B58\u3092\u8FFD\u52A0\
  \u3057\u3066\u304F\u3060\u3055\u3044\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.817001-06:00'
model: gpt-4-1106-preview
summary: "Rust\u3067HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u57FA\u672C\
  \u7684\u306A\u30B3\u30FC\u30C9\u3092\u898B\u3066\u307F\u307E\u3057\u3087\u3046\u3002\
  `reqwest`\u30AF\u30EC\u30FC\u30C8\u3092\u4F7F\u3046\u3068\u7C21\u5358\u3067\u3059\
  \u3002Cargo.toml\u306B\u4F9D\u5B58\u3092\u8FFD\u52A0\u3057\u3066\u304F\u3060\u3055\
  \u3044."
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
weight: 44
---

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
