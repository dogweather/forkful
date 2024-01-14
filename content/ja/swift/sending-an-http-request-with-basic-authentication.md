---
title:                "Swift: HTTPリクエストを基本認証付きで送信する方法"
simple_title:         "HTTPリクエストを基本認証付きで送信する方法"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## なぜ

HTTPリクエストを基本認証で送信するようにソースコードを作成する理由は、サーバーからデータを取得する必要があるためです。基本認証は、一般的に安全な認証方法の1つであり、主にユーザー名とパスワードを使用します。

## 方法

まず、`URLSession`クラスを使用してHTTPリクエストを作成し、`URLRequest`オブジェクトを作成します。次に、`URLRequest`オブジェクトに基本認証情報を追加し、`URLSession`の`dataTask`メソッドを使用してリクエストを送信します。

```
let urlString = "https://example.com/data"
let url = URL(string: urlString)
var request = URLRequest(url: url!)
        
let username = "username"  // ここにユーザー名を入力
let password = "password"  // ここにパスワードを入力
let loginString = String(format: "%@:%@", username, password)
let loginData = loginString.data(using: String.Encoding.utf8)
let base64LoginString = loginData!.base64EncodedString()

request.httpMethod = "GET"  // リクエストのメソッドを設定
request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")  // 基本認証を追加

let task = URLSession.shared.dataTask(with: request) { (data, response, error) in
    if let error = error {
        print("Error: \(error.localizedDescription)")
    }
    else if let data = data, let response = response as? HTTPURLResponse {
        print("Response Status Code: \(response.statusCode)")
        print("Response Data: \(String(data: data, encoding: .utf8)!)")
    }
}
task.resume()  // リクエストを送信
```

上記のコードでは、`URLSession`で作成された`dataTask`の処理が実行され、`response`オブジェクトにはHTTPステータスコードが含まれ、`data`にはサーバーからのレスポンスデータが含まれます。`if let else if let`文を使用して、エラーが発生したかどうかを確認し、レスポンスが正しく取得されたかどうかを確認します。

## ディープダイブ

基本認証は、HTTPリクエストにパスワードを含めるのではなく、暗号化された認証ヘッダーを使用することで、セキュリティを確保します。また、基本認証は、単純でエラーハンドリングが簡単なため、一般的によく使用される認証方法です。

## See Also

- [HTTP Authentication: Basic and Digest Access Authentication in Swift](https://blog.addjam.com/basic-and-digest-access-authentication-in-swift/)
- [How to Use the URLSession to Make HTTP Requests in Swift](https://www.appcoda.com/urlsession-swift/)
- [Authentication Using URL Session in Swift](https://medium.com/@rodrigo_merino/authentication-using-url-session-in-swift-874f6778b7c8)