---
date: 2024-01-20 18:03:04.179274-07:00
description: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306B\u30D9\u30FC\u30B7\u30C3\u30AF\
  \u8A8D\u8A3C\u3092\u4ED8\u3051\u308B\u306E\u306F\u3001\u30B5\u30FC\u30D0\u30FC\u3078\
  \u5B89\u5168\u306B\u30C7\u30FC\u30BF\u3092\u9001\u308B\u65B9\u6CD5\u3067\u3059\u3002\
  \u8A8D\u8A3C\u60C5\u5831\u3067\u30E6\u30FC\u30B6\u30FC\u3092\u8B58\u5225\u3057\u30A2\
  \u30AF\u30BB\u30B9\u3092\u8A31\u53EF\u3059\u308B\u305F\u3081\u306B\u4F7F\u3044\u307E\
  \u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.613479-06:00'
model: gpt-4-1106-preview
summary: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306B\u30D9\u30FC\u30B7\u30C3\u30AF\u8A8D\
  \u8A3C\u3092\u4ED8\u3051\u308B\u306E\u306F\u3001\u30B5\u30FC\u30D0\u30FC\u3078\u5B89\
  \u5168\u306B\u30C7\u30FC\u30BF\u3092\u9001\u308B\u65B9\u6CD5\u3067\u3059\u3002\u8A8D\
  \u8A3C\u60C5\u5831\u3067\u30E6\u30FC\u30B6\u30FC\u3092\u8B58\u5225\u3057\u30A2\u30AF\
  \u30BB\u30B9\u3092\u8A31\u53EF\u3059\u308B\u305F\u3081\u306B\u4F7F\u3044\u307E\u3059\
  \u3002."
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
