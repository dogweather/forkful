---
title:                "基本認証を使用してhttpリクエストを送信する"
html_title:           "C#: 基本認証を使用してhttpリクエストを送信する"
simple_title:         "基本認証を使用してhttpリクエストを送信する"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Rustでの基本認証付きHTTPリクエストの送信
---
## 何となぜ?

HTTPリクエストに基本認証を付けるとは、ユーザー名とパスワードを使ってウェブページにアクセスする手段です。これは、特定のリソースに対する不正なアクセスを防ぐために行われます。

## 手順:

Rustでは、`reqwest`というクレートを使用して基本認証付きHTTPリクエストを簡単に送信できます。

```Rust 
use reqwest::blocking::Client;
use reqwest::Error;

fn main() -> Result<(), Error> {
    let client = Client::new();
    let res = client.get("http://httpbin.org/basic-auth/user/pass")
        .basic_auth("user", Some("pass"))
        .send()?;
        
    println!("{}", res.status());
        
    Ok(())
}
```

これは、基本認証付きのGETリクエストを `http://httpbin.org/basic-auth/user/pass` に送信し、その結果を出力します。

## 深堀り:

歴史的な背景としては、基本認証が初めて RFC 7617 で定義され、その後、HTTP/1.0とHTTP/1.1で広く使用されました。 

基本認証には利点と欠点があります。そのシンプルさゆえに、実装も簡単な反面、パスワードが暗号化されずに平文で送信されるため、安全性の面で弱点があります。

Rustの世界では、基本認証はユーザー名とパスワードを含む `Authorization `ヘッダーをHTTPリクエストに追加する事で可能になります。そして、これには先ほど紹介した `reqwest`の他にも、`hyper`,`actix-web`,`warp`などのクレートが使用可能です。

## 参考資料:

- [Reqwest クレートの公式ドキュメント](https://docs.rs/reqwest)
- [RFC 7617 - Basic Authentication](https://tools.ietf.org/html/rfc7617)