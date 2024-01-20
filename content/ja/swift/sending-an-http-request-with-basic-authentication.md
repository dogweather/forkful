---
title:                "基本認証を使用してhttpリクエストを送信する"
html_title:           "C#: 基本認証を使用してhttpリクエストを送信する"
simple_title:         "基本認証を使用してhttpリクエストを送信する"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

Title: HTTPリクエストの基本的な認証を通じたSwiftプログラミング

## 何 & なぜ?
HTTPリクエストの基本的な認証は、ユーザー名とパスワードを使って、リクエストするサーバーに対するアクセスを認証するものです。これは、データのセキュリティとプライバシーを保護するためにプログラマーが行います。

## 実装方法：
以下に、HTTPリクエストの基本的な認証をSwiftで送る方法の例を述べる。

```Swift
import Foundation

let username = "yourUsername"
let password = "yourPassword"

let loginString = "\(username):\(password)"
let loginData = loginString.data(using: String.Encoding.utf8)!
let base64LoginString = loginData.base64EncodedString()

// リクエストを作成
var request = URLRequest(url: URL(string: "http://www.example.com")!)
request.httpMethod = "POST"
request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")

let task = URLSession.shared.dataTask(with: request) { data, response, error in 
  if let error = error {
    print("Error: \(error)")
  } else if let data = data {
    let str = String(data: data, encoding: .utf8)
    print("Received data:\n\(str ?? "")")
  }
}

task.resume()
```

出力結果：

```
Received data:
(Here would be the data received from www.example.com)
```

## 詳細な情報：
1. **歴史的背景**: ベーシック認証は、HTTP/1.0から存在しています。セキュアでない環境で使用すると、パスワードが平文で送信されるため、安全性に問題がありました。それに対応して、現在では多くの場合HTTPSと一緒に使用されます。
2. **代替手段**: 代替の認証スキームとしては、Bearer認証（トークンベース）、Digest認証（MD5ハッシュを使用）などがあります。
3. **実装詳細**: SwiftのFoundationフレームワークは、URLSessionクラスを通じてHTTPリクエストを行う機能を提供します。この例では、`URLSession.shared.dataTask`メソッドを使用してHTTPリクエストを送信し、クロージャを使って応答を処理しています。

## 参考資料: 
1. [Apple Developer Documentation - URLSession](https://developer.apple.com/documentation/foundation/urlsession)
2. [Using Basic Authentication with URL Session's dataTask()](https://www.xee.ai/2018/12/17/basic-authentication-with-urlsession/)
3. [HTTP Basic Authentication - Mozilla](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)