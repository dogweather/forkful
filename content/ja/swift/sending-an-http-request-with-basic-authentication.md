---
title:                "基本認証を使用して http リクエストを送信する方法"
html_title:           "Swift: 基本認証を使用して http リクエストを送信する方法"
simple_title:         "基本認証を使用して http リクエストを送信する方法"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 何＆なぜ？
HTTPリクエストを基本認証付きで送信することは、プログラマーがウェブサイトやアプリケーションにアクセスするために使用する一般的な方法です。基本認証は、ユーザー名とパスワードを送信することによってサイトまたはアプリケーションに認証を行うセキュリティ機能です。

## 方法：
Swiftで基本認証付きのHTTPリクエストを送信するには、まず```URLRequest```オブジェクトを作成し、認証情報を設定する必要があります。次に、```URLSession```を使用してリクエストを送信します。以下は、基本認証を使用して公式のTwitterアカウントのツイートを取得する例です。

```Swift
// URLRequestを作成する
let request = URLRequest(url: URL(string: "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=swiftlang")!)

// 認証情報を設定する
let authString = "username:password".data(using: .utf8)?.base64EncodedString()
request.setValue("Basic \(authString)", forHTTPHeaderField: "Authorization")

// URLSessionでリクエストを送信し、レスポンスを処理する
let session = URLSession.shared
session.dataTask(with: request) { (data, response, error) in
    guard let data = data else { return }
    // レスポンスを処理する
}.resume()
```

## 詳細を深く掘り下げる：
基本認証は、ウェブの認証方法としては古くから使われてきましたが、セキュリティ上の問題や新しい認証方式の登場により、最近では推奨されていません。代わりに、OAuthなどのより安全な認証方式が推奨されています。また、基本認証はパスワードを平文で送信するため、中間者攻撃による情報漏洩のリスクがあります。そのため、SSLを使用して通信を暗号化することが重要です。

## 関連リンク：
- [Apple Developer Documentation: URLRequest](https://developer.apple.com/documentation/foundation/urlrequest)
- [Apple Developer Documentation: URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [OWASP: Basic Authentication](https://owasp.org/www-community/Basic_authentication)
- [OWASP: The Need for HTTPS](https://owasp.org/www-project-secure-communication/)