---
title:                "「HTTPリクエストを送信する」"
html_title:           "Rust: 「HTTPリクエストを送信する」"
simple_title:         "「HTTPリクエストを送信する」"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

今回は、Rustプログラミング言語でのHTTPリクエスト送信について紹介します。HTTPリクエストとは、Webサーバーから情報を取得するために利用されるもので、プログラマーにとっては非常に重要な機能です。

## なぜHTTPリクエストを送信するのか？
プログラムでHTTPリクエストを送信する理由はいくつかあります。一つは、Webサーバーからデータを取得するためです。例えば、あるWebサイトのデータをプログラムで取得して処理することができます。また、Webサービスを作成する際にもHTTPリクエストを送信することで、クライアントからのリクエストに対応することができます。

## 送信方法
```Rust
// インポート
use std::io::{self, Write};
use std::net::TcpStream;

fn main() {
    // サーバーアドレスとポートを指定
    let address = "google.com";
    let port = "80";

    // ソケットを作成
    let mut stream = TcpStream::connect(format!("{}:{}", address, port)).expect("Could not connect to server");

    // HTTPリクエストを送信
    stream.write(b"GET / HTTP/1.0\r\n\r\n").unwrap();

    // サーバーからのレスポンスを表示
    let mut response = String::new();
    stream.read_to_string(&mut response).expect("Failed to read response");
    println!("{}", response);
}
```

実行すると、GoogleのホームページのHTMLコードが表示されます。

## 詳細情報
HTTPリクエストについての歴史的な背景は、1991年にティム・バーナーズ＝リーによって考案されたHTTPプロトコルにさかのぼります。また、CやPythonなどの他のプログラミング言語でもHTTPリクエストを送信することができますが、Rustのようなコンパイル型言語では、高速かつ安全にHTTPリクエストを処理することができます。

HTTPリクエストは、TCPやTLSなどのプロトコルを使用して送信され、URLやメソッド、ヘッダー情報などで構成されています。Rustでは、上記のコードのようにTcpStreamを使用して、サーバーに接続し、リクエストを送信することができます。

## 関連情報
- [Rust公式ドキュメント](https://doc.rust-lang.org/std/net/struct.TcpStream.html)
- [HTTPプロトコルの歴史](https://developer.mozilla.org/ja/docs/Web/HTTP/Background_-_A_brief_history_of_HTTP)