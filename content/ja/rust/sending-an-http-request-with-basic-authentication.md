---
title:                "基本認証付きのhttpリクエストの送信"
html_title:           "Rust: 基本認証付きのhttpリクエストの送信"
simple_title:         "基本認証付きのhttpリクエストの送信"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 何 & なぜ？
HTTPリクエストを基本認証付きで送信するとは、ユーザー名とパスワードを入力してWebサービスにアクセスすることを意味します。プログラマーがこの方法を使用する理由は、安全性を確保し、権限を持つユーザーのみがサービスにアクセスできるようにするためです。

## 方法：
```Rust
use reqwest::blocking;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let client = blocking::Client::new();
    let res = client
        .get("https://example.com")
        .basic_auth("username", Some("password"))
        .send()?;
    println!("Response Status: {}", res.status());
    println!("Response Body: {}", res.text()?);
    Ok(())
}
```

## 深堀り：
基本認証は、1999年にHTTPの標準仕様の一部として導入されました。代替手段として、OAuthやOpenIDなどの認証プロトコルがあります。基本認証の実装には、認証ヘッダーをHTTPリクエストに追加する必要があります。RustのReqwestライブラリは、この処理を簡単にする便利なメソッドを提供しています。

## 関連サイト：
- [HTTP Basic認証のドキュメンテーション](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)
- [RustのReqwestライブラリのドキュメント](https://docs.rs/reqwest)
- [OAuthやOpenIDについての詳細](https://www.oauth.com/)