---
title:                "HTTPリクエストの送信"
html_title:           "Swift: HTTPリクエストの送信"
simple_title:         "HTTPリクエストの送信"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 何が？どうして？

HTTPリクエストを送るとは、インターネット上でデータを送受信するための方法です。プログラマーは、ウェブサイトのデータやサーバーとの通信を処理するために、HTTPリクエストを使用します。

## 手順：

まず、HTTPリクエストを送るためにはURL（ウェブサイトのアドレス）が必要です。次に、URLを使用してリクエストを作成し、HTTPメソッドを指定します。最後に、サーバーからのレスポンスを処理します。

```Swift
// URLを設定する
let url = URL(string: "https://example.com")!

// HTTPリクエストを作成する
var request = URLRequest(url: url)

// HTTPメソッドを指定する
request.httpMethod = "GET"

// サーバーからのレスポンスを処理する
URLSession.shared.dataTask(with: request) { data, response, error in
    guard let data = data, error == nil else {
        print(error?.localizedDescription ?? "Unknown error")
        return
    }
    print(String(data: data, encoding: .utf8)!)
}.resume()
```

上記のコードでは、URLを設定し、GETメソッドを使用してサーバーにリクエストを送信しています。レスポンスにはデータが含まれているので、`response`を使用してデータの処理を行っています。

## 詳細を見る：

HTTPリクエストは、1990年代に開発された通信プロトコルです。代表的なバージョンはHTTP/1.1であり、最近ではHTTP/2やHTTP/3といった改良版が開発されています。

代替手段として、WebSocketやgRPCといったプロトコルがありますが、HTTPリクエストがまだ最もポピュラーな方法です。また、`URLSession`クラスを使用することで、HTTPリクエストをより簡単に実装することができます。

## 関連情報を見る：

- [HTTPメソッドとは？](https://developer.mozilla.org/ja/docs/Web/HTTP/Methods)
- [URLSessionでHTTPリクエストを行う方法](https://developer.apple.com/documentation/foundation/urlsession)
- [WebSocketとは？](https://developer.mozilla.org/ja/docs/Web/API/WebSocket)
- [gRPCとは？](https://grpc.io/)