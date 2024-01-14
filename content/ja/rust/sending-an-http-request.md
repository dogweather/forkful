---
title:                "Rust: 「HTTPリクエストの送信」"
simple_title:         "「HTTPリクエストの送信」"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## なぜ？

Rustは高速で安全性の高いプログラミング言語ですが、最近多くのWeb開発者がRustを使ってHTTPリクエストを送信する方法を学び始めました。なぜそうするのでしょうか？それについて説明していきましょう。

## 使い方

まず始めに、RustでHTTPリクエストを送信するためには、[reqwest](https://github.com/seanmonstar/reqwest)というライブラリを使う必要があります。そのためには、Cargo.tomlファイルに以下のように追加します。

```Rust
[dependencies]
reqwest = "0.11.1"
```

次に、以下のようにサンプルコードを書きます。

```Rust
use reqwest::Error;
use std::collections::HashMap;
use serde::{Deserialize, Serialize};

#[derive(Debug, Deserialize, Serialize)]
struct User {
    name: String,
    age: u32,
    email: String,
}

async fn send_request() -> Result<(), Error> {
    let client = reqwest::Client::new();
    let mut map = HashMap::new();
    map.insert("name", "John");
    map.insert("age", "25");
    map.insert("email", "john@example.com");
    let request = client.post("https://example.com/users")
        .json(&map)
        .send()
        .await?;
    let response = request.json::<User>().await?;
    println!("{:#?}", response);
    Ok(())
}

#[tokio::main]
async fn main() -> Result<(), Error> {
    send_request().await?;
    Ok(())
}
```

コードを実行すると、以下のような出力が得られるでしょう。

```Rust
User {
    name: "John",
    age: 25,
    email: "email@example.com",
}
```

## ディープダイブ

深く掘り下げてみましょう。reqwestライブラリは、async/awaitパターンに対応したノンブロッキングIOを使用しており、効率的にHTTPリクエストを送信することができます。また、HTTPリクエストの設定や、ヘッダーの追加など、さまざまなオプションをカスタマイズすることもできます。詳細なドキュメントは[公式ドキュメント](https://docs.rs/reqwest/0.11.1/reqwest/)を参照してください。

## 参考リンク

- [RustでHTTPリクエストを送信する方法 | Ja.me Rust](https://ja.me/rust-send-http-request)
- [reqwest library - Github](https://github.com/seanmonstar/reqwest)
- [async/await in Rust - Rust-Lang Learning](https://learning-rust.github.io/docs/e6.async-await/)