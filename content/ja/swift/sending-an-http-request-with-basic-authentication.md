---
title:                "基本認証を使用してhttpリクエストを送信する方法"
html_title:           "Swift: 基本認証を使用してhttpリクエストを送信する方法"
simple_title:         "基本認証を使用してhttpリクエストを送信する方法"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## なぜ
HTTPリクエストを基本認証で送信する理由は、アクセス制限が必要なWebアプリケーションやAPIへのアクセスを許可するためです。

## 方法
基本認証を使用してHTTPリクエストを送信するには、Swiftの`URLSession`クラスを使用します。以下のコード例を参考にしてください。

```Swift
let urlString = "https://example.com/api"
let url = URL(string: urlString)

// リクエストを作成
var request = URLRequest(url: url!)

// HTTPメソッドを設定
request.httpMethod = "GET"

// 認証情報を設定
let user = "username"
let password = "password"
let loginString = String(format: "%@:%@", user, password)
let loginData = loginString.data(using: .utf8)
let base64LoginString = loginData?.base64EncodedString()
request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")

// セッションを作成
let session = URLSession(configuration: .default)

// リクエストを送信
let task = session.dataTask(with: request) { (data, response, error) in
    if let error = error {
        print("Error: \(error.localizedDescription)")
    }
    if let response = response as? HTTPURLResponse {
        // レスポンスステータスコードをチェック
        if response.statusCode == 200 {
            if let data = data {
                // レスポンスデータを処理
                print("Response data: \(data)")
            }
        } else {
            print("Server error: \(response.statusCode)")
        }
    }
}

// リクエストを開始
task.resume()
```
上記のコードでは、`URLSession`クラスの`dataTask(with:completionHandler:)`メソッドを使用してリクエストを送信し、レスポンスを受け取っています。認証情報を含めるために、`URLRequest`クラスの`setValue(_:forHTTPHeaderField:)`メソッドを使用してヘッダーに`Authorization`を追加しています。

## 深堀り
基本認証は、ユーザー名とパスワードをBase64でエンコードしてリクエストのヘッダーに含める認証方式です。この認証方式は安全性が低いため、SSLやHTTPSなどの別のセキュリティプロトコルを併用することが推奨されています。

## 参考リンク
- [URLSession - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/urlsession)
- [HTTP Basic Access Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_access_authentication)
- [Base64 - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/base64encoding)