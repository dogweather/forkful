---
title:                "HTTPリクエストの送信"
date:                  2024-01-20T18:00:32.530437-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTPリクエストの送信"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/sending-an-http-request.md"
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