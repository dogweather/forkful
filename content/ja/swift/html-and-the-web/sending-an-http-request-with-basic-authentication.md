---
date: 2024-01-20 18:03:04.179274-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T22:50:56.492089-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) HTTP\u30D9\u30FC\u30B7\u30C3\u30AF\u8A8D\u8A3C\u306F\u3001\
  RFC 7617\u3067\u5B9A\u7FA9\u3055\u308C\u3066\u3044\u308B\u30B7\u30F3\u30D7\u30EB\
  \u306A\u8A8D\u8A3C\u30B9\u30AD\u30FC\u30E0\u3067\u3059\u3002\u30E6\u30FC\u30B6\u30FC\
  \u540D\u3068\u30D1\u30B9\u30EF\u30FC\u30C9\u3092\u30B3\u30ED\u30F3\u3067\u7E4B\u3052\
  \u3001Base64\u3067\u30A8\u30F3\u30B3\u30FC\u30C9\u3059\u308B\u3053\u3068\u3067\u8A8D\
  \u8A3C\u60C5\u5831\u3092\u4F5C\u308A\u307E\u3059\u3002\u3057\u304B\u3057\u3001HTTP\u30D9\
  \u30FC\u30B7\u30C3\u30AF\u8A8D\u8A3C\u306F\u5B89\u5168\u3067\u306F\u306A\u3044\u3068\
  \u3055\u308C\u3066\u304A\u308A\u3001HTTPS\u306E\u4F7F\u7528\u304C\u63A8\u5968\u3055\
  \u308C\u307E\u3059\u3002\u5B89\u5168\u6027\u304C\u6C42\u3081\u3089\u308C\u308B\u5834\
  \u5408\u3001OAuth\u3084JWT\u306A\u3069\u306E\u4EE3\u66FF\u624B\u6BB5\u304C\u3042\
  \u308A\u307E\u3059\u3002\u307E\u305F\u3001iOS\u3067\u306F`URLSession`\u3092\u4F7F\
  \u3063\u3066HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u7C21\u5358\u306B\u6271\u3046\
  \u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002"
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F7F\u7528\u3057\u305FHTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u306E\u9001\u4FE1"
weight: 45
---

## How to: (方法)
```Swift
import Foundation

// ユーザー名とパスワードを準備します。
let username = "user"
let password = "password"

// ベーシック認証の文字列を生成します。
if let loginData = "\(username):\(password)".data(using: .utf8) {
    let base64LoginString = loginData.base64EncodedString()
  
    // リクエストを作り、ヘッダーに認証情報を追加します。
    if let url = URL(string: "https://example.com/api/data") {
        var request = URLRequest(url: url)
        request.httpMethod = "GET"
        request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")
  
        // リクエストを送信します。
        let task = URLSession.shared.dataTask(with: request) { data, response, error in
            // エラーハンドリング
            if let error = error {
                print("Client error: \(error.localizedDescription)")
                return
            }
  
            guard let httpResponse = response as? HTTPURLResponse,
                  (200...299).contains(httpResponse.statusCode) else {
                print("Server error!")
                return
            }
  
            // 応答データを処理します。
            if let mimeType = httpResponse.mimeType, mimeType == "application/json",
               let data = data,
               let dataString = String(data: data, encoding: .utf8) {
                print("Got data: \(dataString)")
            }
        }
        task.resume()
    }
}
```
サンプル出力:

```
Got data: {"example":"data"}
```

## Deep Dive (深掘り)
HTTPベーシック認証は、RFC 7617で定義されているシンプルな認証スキームです。ユーザー名とパスワードをコロンで繋げ、Base64でエンコードすることで認証情報を作ります。しかし、HTTPベーシック認証は安全ではないとされており、HTTPSの使用が推奨されます。安全性が求められる場合、OAuthやJWTなどの代替手段があります。また、iOSでは`URLSession`を使ってHTTPリクエストを簡単に扱うことができます。

## See Also (関連情報)
- [HTTP Basic Auth on MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication_scheme)
- [RFC 7617, The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
- [URLSession on Apple Developer Documentation](https://developer.apple.com/documentation/foundation/urlsession)
