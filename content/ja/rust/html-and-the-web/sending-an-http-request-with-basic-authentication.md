---
date: 2024-01-20 18:03:00.729226-07:00
description: "\u5B9F\u88C5\u65B9\u6CD5 (How to:) Rust\u3067\u306E\u30B3\u30FC\u30C9\
  \u30B5\u30F3\u30D7\u30EB\u3067\u3059\u3002\u3053\u3053\u3067\u306F`reqwest`\u30AF\
  \u30EC\u30FC\u30C8\u3092\u4F7F\u3044\u3001Basic\u8A8D\u8A3C\u4ED8\u304D\u306EHTTP\
  \ GET\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308A\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.723584-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F7F\u7528\u3057\u305FHTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u306E\u9001\u4FE1"
weight: 45
---

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
