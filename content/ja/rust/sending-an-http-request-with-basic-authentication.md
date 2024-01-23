---
title:                "基本認証を使用したHTTPリクエストの送信"
date:                  2024-01-20T18:03:00.729226-07:00
model:                 gpt-4-1106-preview
simple_title:         "基本認証を使用したHTTPリクエストの送信"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 何となぜ？ (What & Why?)
HTTPリクエストをBasic認証付きで送ることは、ユーザー名とパスワードを使ってサーバーに安全にアクセスする方法です。サーバーが要求者の身元を確認し、許可されたユーザーだけがリソースにアクセスできるようにするために使います。

## 実装方法 (How to:)
Rustでのコードサンプルです。ここでは`reqwest`クレートを使い、Basic認証付きのHTTP GETリクエストを送ります。

```Rust
use reqwest::header::{Authorization, Basic};
use std::collections::HashMap;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let client = reqwest::Client::new();
    let user = "username";  // あなたのユーザー名
    let pass = "password";  // あなたのパスワード
    let auth = Basic {
        username: user.to_string(),
        password: Some(pass.to_string()),
    };

    let res = client.get("http://example.com")
        .header(Authorization(auth))
        .send()
        .await?;

    let status = res.status();
    let headers = res.headers().clone();
    let body = res.text().await?;

    println!("Status: {}", status);
    println!("Headers:\n{:?}", headers);
    println!("Body:\n{}", body);

    Ok(())
}
```
このコードを実行すると、レスポンスのステータスコード、ヘッダー、ボディがコンソールに表示されます。

## 詳細解説 (Deep Dive)
Basic認証は、RFC 7617で定義されており、最もシンプルなHTTP認証の方法の一つです。ヘッダーに`Authorization`を含め、`base64`でエンコードされたユーザー名とパスワードをサーバーに送ります。ただし、HTTPSを使わなければ、第三者による簡単な傍受が可能なので、安全性には注意が必要です。

代わりにトークンベースの認証やOAuthなどのより安全な認証方法もありますが、簡単さや互換性のためにBasic認証が使われることもあります。

具体的な実装では、Rustの強力な型システムとエラーハンドリングを使って、不正なレスポンスやネットワークエラーを適切に扱うことができます。`reqwest`クレートは、非同期のHTTPリクエストを簡単に送れる高レベルのAPIを提供しています。

## 参考資料 (See Also)
- [reqwest crate documentation](https://docs.rs/reqwest/)
- [The Rust async book](https://rust-lang.github.io/async-book/)
- [RFC 7617, The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
- [Rust Cookbook - HTTP Clients](https://rust-lang-nursery.github.io/rust-cookbook/web/clients.html)
