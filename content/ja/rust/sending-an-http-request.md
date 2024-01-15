---
title:                "http リクエストの送信"
html_title:           "Rust: http リクエストの送信"
simple_title:         "http リクエストの送信"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## なぜ

ネットワーク通信をするとき、多くのアプリやサイトはHTTPリクエストを使います。これにより、サーバーから情報を取得したり、データを送信したりすることができます。

## 使い方

```Rust
use reqwest::blocking::Client;

fn main() {
    let client = Client::new();
    let res = client.get("https://example.com")
        .send()
        .expect("Failed to send request");
        
    println!("Response status: {}", res.status());
    println!("Response body: {}", res.text().unwrap());
}
```

上のコードでは、 `reqwest` ライブラリを使用してHTTPリクエストを送信しています。まず、`Client` を作成し、`get()` メソッドを使用してリクエスト先のURLを指定します。そして、`send()` メソッドを呼び出して実際にリクエストを送信します。レスポンス情報を取得するには、`status()` メソッドや`text()` メソッドを使用します。 

## 深堀り

HTTPリクエストを送信するとき、リクエストメソッドとしてGETやPOSTなどを指定することができます。また、ヘッダー情報を追加したり、リクエストボディを設定したりすることも可能です。詳細な使い方やパラメーターの設定方法については、[公式ドキュメント](https://docs.rs/reqwest/0.11.4/reqwest/)を参照してください。

## 関連リンク

- [RustでHTTPリクエストを送信する方法](https://qiita.com/hnakamur/items/bc6e4685854bee2ba040)
- [reqwestライブラリの使い方](https://qiita.com/haltafin/items/042237fe6e318ea3190e)
- [Rustを使ってREST APIを叩いてみる](https://qiita.com/laqiiz/items/96f96b91d0f8175355d6)