---
title:                "HTTPリクエストの送信"
html_title:           "Bash: HTTPリクエストの送信"
simple_title:         "HTTPリクエストの送信"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 何となぜ？

HTTPリクエストを送信する事は、情報をWebサーバーに要求・送信するためのプロセスであり、プログラマーがデータを取得、更新、削除するためにご利用します。

## 使い方:

`reqwest` クレートを使ってRustでHTTPリクエストを送る基本的な例を以下に示します。

まず、Cargo.tomlファイルに`reqwest` クレートを追加します。

```Rust
[dependencies]
reqwest = "0.10"
```

そして以下のコードでHTTPリクエストを送ります。

```Rust
use reqwest;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let response = reqwest::get("https://httpbin.org/get").await?;

    println!("{}", response.text().await?);

    Ok(())
}
```

出力例：

```Rust
{
  "args": {}, 
  "headers": {
    "x-sent-from": "Rust reqwest"
  }, 
  "origin": "111.111.1.11", 
  "url": "https://httpbin.org/get"
}
```

## 深堀り:

RustでHTTPリクエストを送るためのライブラリはいくつかあります。`reqwest`は非同期プログラミングをサポートし、使いやすさを重視した選択肢です。

また、情報交換のための基本的なHTTP以外にも、WebSocketやgRPCなどの選択肢もあります。 

HTTPリクエストの過程をより深く理解するには、HTTPプロトコルの歴史とその作業の詳細について学ぶとよいです。リクエストが何を行い、サーバーがそれにどのように応答するかを理解すると、あなたのコードがどのように動作するのかをよりよく理解することができます。

## 参考リンク：

Reqwestのドキュメンテーション：https://docs.rs/reqwest/

HTTPとRESTについての詳細：https://developer.mozilla.org/ja/docs/Web/HTTP

Rustによる非同期プログラミング：https://rust-lang.github.io/async-book/01_getting_started/01_chapter.html

HTTPプロトコルのヒストリー：https://developer.mozilla.org/ja/docs/Web/HTTP/Overview

以上がRustでHTTPリクエストを送信する方法についての基本的なガイドです。この情報があなたのプログラミングに役立つことを願っています。