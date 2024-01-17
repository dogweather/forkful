---
title:                "「HTTPリクエストの送信」"
html_title:           "Haskell: 「HTTPリクエストの送信」"
simple_title:         "「HTTPリクエストの送信」"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 送信するHTTPリクエストとは？

HTTPリクエストを送信するとは何かの説明をすると、コンピュータやネットワークプログラマーがウェブサーバーと通信するための方法です。プログラマーたちはHTTPリクエストを送信することで、ウェブサーバーから情報を取得したり、ウェブサービスを使用したりすることができます。

## 方法：

```Haskell
import Network.HTTP
main = do
    resp <- simpleHTTP (getRequest "http://www.example.com")
    let body = fmap rspBody resp
    print body
```

上記のコードは、HTTPライブラリを使用してウェブサイトのページを取得し、そのページのボディを表示するものです。コードを実行すると、ウェブサイト「www.example.com」のHTMLコンテンツが表示されます。

## 深い掘り下げ：

HTTPリクエストは、現在のウェブ標準であるHTTPプロトコルに基づいています。以前は、HTTPよりも複雑なプロトコルであるFTPやTelnetがよく使われていましたが、今ではそれらはほとんど利用されていません。

HTTPリクエストを送信するために、Haskellには他のライブラリやパッケージもあります。例えばHTTPコンビネータライブラリやHTTPクライアントライブラリがありますが、今回紹介したライブラリはシンプルで使いやすく、多くの機能を提供してくれます。

## 関連：

HaskellでHTTPリクエストを送信する方法は他にも多数あります。もし今回紹介したライブラリがあなたに合わない場合は、以下のリンクを参考にしてみてください。

- [Hackage - HTTPライブラリ一覧](https://hackage.haskell.org/packages/search?terms=HTTP)

- [r/haskell - Simple HTTPクライアントを使用してリクエストを送信する方法](https://www.reddit.com/r/haskell/comments/m8rm6/how_to_send_request_using_simple_http_client/)