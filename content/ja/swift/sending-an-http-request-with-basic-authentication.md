---
title:                "基本認証を使用したHTTPリクエストの送信"
date:                  2024-01-20T18:03:04.179274-07:00
model:                 gpt-4-1106-preview
simple_title:         "基本認証を使用したHTTPリクエストの送信"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

HTTPリクエストにベーシック認証を付けるのは、サーバーへ安全にデータを送る方法です。認証情報でユーザーを識別しアクセスを許可するために使います。

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