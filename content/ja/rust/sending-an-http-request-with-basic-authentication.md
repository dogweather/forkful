---
title:                "基本認証を使用したhttpリクエストの送信"
html_title:           "Rust: 基本認証を使用したhttpリクエストの送信"
simple_title:         "基本認証を使用したhttpリクエストの送信"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## なぜ

HTTPリクエストを基本認証で送ることに関心があるのか？それは、APIやWebサービスにアクセスする際に必要となるセキュリティ認証の一つであり、プログラマーにとって重要なスキルであるからです。

## 方法

まず、`reqwest`ライブラリを使ってHTTPリクエストを送るための基本的なコードを見てみましょう。

```Rust
use reqwest::Client;

fn main() {
    // APIエンドポイントのURL
    let url = "https://example.com/api";

    // HTTPクライアントを作成
    let client = Client::new();

    // 基本認証を含むリクエストを作成
    let request = client.get(url)
        .basic_auth("username", Some("password"))
        .send()
        .unwrap();
}
```

上記の例では、APIエンドポイントのURLを指定し、`Client::new()`でHTTPクライアントを作成します。そして、`.basic_auth()`を使ってユーザー名とパスワードを指定し、`send()`でリクエストを実行します。このコードを実行すると、指定したAPIに基本認証を含むHTTPリクエストが送られます。

## ディープダイブ

基本認証にはどのようなメカニズムがあるのか、少し掘り下げてみましょう。基本認証は、HTTPリクエストヘッダーに`Authorization`フィールドを含めることで認証情報をサーバーに送信する方法です。このフィールドには、`Basic`という認証スキームが使用され、ユーザー名とパスワードがBASE64でエンコードされて一緒に送信されます。この認証情報はサーバー側でデコードされ、ユーザー名とパスワードが一致する場合にのみ認証成功となります。

See Also

- [reqwestドキュメント](https://docs.rs/reqwest)
- [RustでAPIリクエストを送る方法](https://dev.classmethod.jp/server-side/rust-api-request/)