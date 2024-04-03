---
date: 2024-01-20 18:00:32.530437-07:00
description: "How to: (\u3069\u306E\u3088\u3046\u306B\u5B9F\u884C\u3059\u308B\u304B\
  ) Swift\u3067HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u65B9\u6CD5\u306F\
  \u3001`URLSession`\u30AF\u30E9\u30B9\u3092\u5229\u7528\u3059\u308B\u3053\u3068\u3067\
  \u3059\u3002\u4EE5\u4E0B\u306FGET\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u30B5\u30F3\
  \u30D7\u30EB\u30B3\u30FC\u30C9\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.608868-06:00'
model: gpt-4-1106-preview
summary: "Swift\u3067HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u65B9\u6CD5\
  \u306F\u3001`URLSession`\u30AF\u30E9\u30B9\u3092\u5229\u7528\u3059\u308B\u3053\u3068\
  \u3067\u3059\u3002\u4EE5\u4E0B\u306FGET\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u30B5\
  \u30F3\u30D7\u30EB\u30B3\u30FC\u30C9\u3067\u3059."
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
weight: 44
---

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
