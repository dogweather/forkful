---
aliases:
- /ja/swift/sending-an-http-request/
date: 2024-01-20 18:00:32.530437-07:00
description: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u3053\u3068\u306F\
  \u3001\u30A4\u30F3\u30BF\u30FC\u30CD\u30C3\u30C8\u4E0A\u306E\u30B5\u30FC\u30D0\u30FC\
  \u306B\u30C7\u30FC\u30BF\u3092\u8981\u6C42\u3059\u308B\u30D7\u30ED\u30BB\u30B9\u3067\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30A6\u30A7\u30D6\u30B3\
  \u30F3\u30C6\u30F3\u30C4\u3092\u53D6\u5F97\u3057\u305F\u308A\u3001\u5916\u90E8API\u3068\
  \u901A\u4FE1\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\
  \u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:55.226547
model: gpt-4-1106-preview
summary: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u3053\u3068\u306F\u3001\
  \u30A4\u30F3\u30BF\u30FC\u30CD\u30C3\u30C8\u4E0A\u306E\u30B5\u30FC\u30D0\u30FC\u306B\
  \u30C7\u30FC\u30BF\u3092\u8981\u6C42\u3059\u308B\u30D7\u30ED\u30BB\u30B9\u3067\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30A6\u30A7\u30D6\u30B3\u30F3\
  \u30C6\u30F3\u30C4\u3092\u53D6\u5F97\u3057\u305F\u308A\u3001\u5916\u90E8API\u3068\
  \u901A\u4FE1\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\
  \u3044\u307E\u3059\u3002"
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
HTTPリクエストを送ることは、インターネット上のサーバーにデータを要求するプロセスです。プログラマーは、ウェブコンテンツを取得したり、外部APIと通信したりするためにこれを行います。

## How to: (どのように実行するか)
SwiftでHTTPリクエストを送る方法は、`URLSession`クラスを利用することです。以下はGETリクエストのサンプルコードです。

```Swift
import Foundation

let url = URL(string: "https://api.example.com/data")!

let task = URLSession.shared.dataTask(with: url) { data, response, error in
    // エラーハンドリング
    if let error = error {
        print("Error: \(error)")
        return
    }
    
    // レスポンスの確認とデータの使用
    if let response = response as? HTTPURLResponse, response.statusCode == 200 {
        if let data = data, let dataString = String(data: data, encoding: .utf8) {
            print("Received data:\n\(dataString)")
        }
    } else {
        print("HTTP Request failed")
    }
}

task.resume()
```

サンプル出力:

```
Received data:
{"example":"This is a JSON response."}
```

## Deep Dive (深掘り)
HTTPリクエストは、ウェブの基盤となり、1990年代初頭に誕生しました。`URLSession`はiOS 7以降に利用可能で、それ以前には`NSURLConnection`が使われていました。代替手段として、サードパーティ製ライブラリ`Alamofire`がありますが、小規模なプロジェクトではSwift標準の機能で十分です。`URLSession`は複数の設定をカスタマイズでき、例えばタイムアウト、キャッシュポリシー、ヘッダーの指定等を行うことができます。

## See Also (関連情報)
- [Apple Developer Documentation: URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [はじめてのSwiftプログラミング](https://www.apple.com/jp/swift/playgrounds/)
- [Alamofire GitHub](https://github.com/Alamofire/Alamofire)
