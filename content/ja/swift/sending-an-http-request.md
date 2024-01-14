---
title:                "Swift: 「httpリクエストの送信」"
simple_title:         "「httpリクエストの送信」"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## なぜ

HTTPリクエストを送信する理由は、Webサイトやアプリケーションとのデータのやりとりを行うためです。例えば、ログイン情報やフォームの入力情報をサーバーに送信する際に使用します。

## 使い方

まずは、SwiftでHTTPリクエストを送信する方法をご紹介します。`URLSession`というクラスが用意されており、URLを指定してリクエストを送信することができます。例を見てみましょう。

```Swift
let urlString = "https://example.com/api/login"
if let url = URL(string: urlString) {
    var request = URLRequest(url: url)
    request.httpMethod = "POST"
    let parameters = ["username": "johnsmith", "password": "123456"]
    request.httpBody = try? JSONSerialization.data(withJSONObject: parameters, options: [])
    let task = URLSession.shared.dataTask(with: request) { data, response, error in
        if let data = data, let dataString = String(data: data, encoding: .utf8) {
            print(dataString)
        }
    }
    task.resume()
}
```

このコードでは、`URLSession`を使用してPOSTリクエストを送信しています。リクエストのURLは、`urlString`で指定し、リクエストのボディには`parameters`という辞書型のデータをJSON形式でセットしています。そして、`URLRequest`クラスを使用してリクエストを作成し、`URLSession`の`dataTask`メソッドを使用してリクエストを実行しています。サーバーからのレスポンスは、`data`として受け取り、`dataString`に変換して表示しています。

## ディープダイブ

HTTPリクエストを完全に理解するためには、リクエストの構造やヘッダーの詳細などを理解する必要があります。また、GETリクエストやPUTリクエストなど、さまざまなタイプのリクエストがあり、それぞれの違いについても理解する必要があります。詳細な情報は、[Appleの公式ドキュメント](https://developer.apple.com/documentation/foundation/url_loading_system)や[Swiftのウェブサイト](https://swift.org/documentation/#the-swift-standard-library)で確認することができます。

## 関連リンク

- [Apple公式ドキュメント](https://developer.apple.com/documentation/foundation/url_loading_system)
- [Swiftのウェブサイト](https://swift.org/documentation/#the-swift-standard-library)
- [SwiftでHTTPリクエストを送信する方法](https://www.hackingwithswift.com/example-code/system/how-to-send-the-http-request-with-swift)