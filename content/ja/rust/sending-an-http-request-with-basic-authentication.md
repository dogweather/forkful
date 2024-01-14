---
title:                "Rust: ベーシック認証を使用してhttpリクエストを送信する"
simple_title:         "ベーシック認証を使用してhttpリクエストを送信する"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# なぜ

HTTPリクエストを基本認証を使って送信する理由は、セキュリティのためです。基本認証は、ユーザーがユーザー名とパスワードを入力することで、リクエストを送信する前に認証される仕組みです。

## 使い方

まずは、"reqwest"ライブラリをインポートします。次に、リクエストを送信するためのURLを準備します。そして、リクエストに必要なヘッダーを作成し、基本認証を使って認証します。最後に、リクエストを送信し、レスポンスを処理します。下記のコードはその一例です。

```Rust
use reqwest::header::HeaderValue;
use reqwest::Client;
use std::collections::HashMap;

// URLを設定
let url = "https://example.com/api/";

// リクエストに必要なヘッダーを作成
let mut headers = HashMap::new();
// ベーシック認証を使用するため、"Authorization"ヘッダーを作成
let username = "your_username";
let password = "your_password";

// ユーザー名とパスワードを結合して、認証用の文字列を作成
let auth_string = format!("{}:{}", username, password);
// Base64エンコードを行い、値をString型に変換
let auth_value = format!("Basic {}", base64::encode(auth_string));

// "Authorization"ヘッダーにエンコードした値を設定
headers.insert("Authorization", HeaderValue::from_str(auth_value.as_str()).unwrap());

// クライアントを作成し、リクエストを送信する
let client = Client::new();
// クライアントにヘッダーを付けてリクエストを作成
let resp = client.get(url).headers(headers).send().await;

// レスポンスを処理する
match resp {
    Ok(resp) => {
        // レスポンスが成功した場合は、リクエストが正しく認証されたことを示す
        println!("Request successfully authorized!");
    },
    Err(e) => {
        // レスポンスがエラーだった場合は、認証に失敗したことを示す
        println!("Failed to authorize request: {}", e);
    },
}
```

## ディープダイブ

基本認証は、HTTPリクエストにおける最も古い認証の仕組みの1つです。しかし、セキュリティ上の問題が指摘され、現在はよりセキュアな認証方法が推奨されています。基本認証は、パスワードが暗号化されていないため、第三者に傍受される可能性があります。また、基本認証を使う場合は、HTTPSを使うことが推奨されます。

基本認証を使用する場合は、ユーザー名とパスワードを保管する方法にも注意を払う必要があります。暗号化やセキュアな保存方法を使うことで、より安全な基本認証を実現できます。

# 参考リンク

- [reqwest - Rust Documentation](https://docs.rs/reqwest/0.11.1/reqwest/)
- [Basic access authentication - Wikipedia](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [基本認証とは？基本認証の仕組みや仕様について解説 | TECHSCORE(テックスコア)](https://www.techscore.com/tech/VBA/HTTP/3-6/)
- [Basic Authentication vs