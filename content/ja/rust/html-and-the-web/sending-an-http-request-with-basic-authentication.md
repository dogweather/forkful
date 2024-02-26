---
date: 2024-01-20 18:03:00.729226-07:00
description: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092Basic\u8A8D\u8A3C\u4ED8\u304D\
  \u3067\u9001\u308B\u3053\u3068\u306F\u3001\u30E6\u30FC\u30B6\u30FC\u540D\u3068\u30D1\
  \u30B9\u30EF\u30FC\u30C9\u3092\u4F7F\u3063\u3066\u30B5\u30FC\u30D0\u30FC\u306B\u5B89\
  \u5168\u306B\u30A2\u30AF\u30BB\u30B9\u3059\u308B\u65B9\u6CD5\u3067\u3059\u3002\u30B5\
  \u30FC\u30D0\u30FC\u304C\u8981\u6C42\u8005\u306E\u8EAB\u5143\u3092\u78BA\u8A8D\u3057\
  \u3001\u8A31\u53EF\u3055\u308C\u305F\u30E6\u30FC\u30B6\u30FC\u3060\u3051\u304C\u30EA\
  \u30BD\u30FC\u30B9\u306B\u30A2\u30AF\u30BB\u30B9\u3067\u304D\u308B\u3088\u3046\u306B\
  \u3059\u308B\u305F\u3081\u306B\u4F7F\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:39.874091-07:00'
model: gpt-4-1106-preview
summary: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092Basic\u8A8D\u8A3C\u4ED8\u304D\u3067\
  \u9001\u308B\u3053\u3068\u306F\u3001\u30E6\u30FC\u30B6\u30FC\u540D\u3068\u30D1\u30B9\
  \u30EF\u30FC\u30C9\u3092\u4F7F\u3063\u3066\u30B5\u30FC\u30D0\u30FC\u306B\u5B89\u5168\
  \u306B\u30A2\u30AF\u30BB\u30B9\u3059\u308B\u65B9\u6CD5\u3067\u3059\u3002\u30B5\u30FC\
  \u30D0\u30FC\u304C\u8981\u6C42\u8005\u306E\u8EAB\u5143\u3092\u78BA\u8A8D\u3057\u3001\
  \u8A31\u53EF\u3055\u308C\u305F\u30E6\u30FC\u30B6\u30FC\u3060\u3051\u304C\u30EA\u30BD\
  \u30FC\u30B9\u306B\u30A2\u30AF\u30BB\u30B9\u3067\u304D\u308B\u3088\u3046\u306B\u3059\
  \u308B\u305F\u3081\u306B\u4F7F\u3044\u307E\u3059\u3002"
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F7F\u7528\u3057\u305FHTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u306E\u9001\u4FE1"
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
