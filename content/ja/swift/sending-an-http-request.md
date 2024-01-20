---
title:                "HTTPリクエストの送信"
html_title:           "Bash: HTTPリクエストの送信"
simple_title:         "HTTPリクエストの送信"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

# SwiftでHTTPリクエストを送信する

## 何これ、なんで？

HTTPリクエストを送信するとは、ウェブサーバーに情報を要求または送信する行為です。プログラマーは通常、データ取得やデータ操作のためにこれを行います。

## どうやるか

Swiftには、HTTPリクエストを送信するために利用できるURLRequestというクラスがあります。例えば以下のようなコードで、GETリクエストを送信できます:

```Swift
import Foundation

let url = URL(string: "https://www.example.com")!
var request = URLRequest(url: url)
request.httpMethod = "GET"
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
このコードは、指定したURLにGETリクエストを送信し、取得したデータまたはエラーをコンソールに印刷します。

## より深く知る

HTTPリクエストの送信は、ウェブプログラミングの基礎となる概念です。これはウェブの成立以来、サーバーとクライアント間のデータ交換の基盤を成しています。

Swift以外の言語でもHTTPリクエストの送信方法はよく似ています。例えば、JavaScriptでは`fetch()`関数、Pythonでは`requests`ライブラリを利用します。つまり、1つの言語でリクエストの送信方法を理解すれば、他の言語でもそれを容易に適用できます。

Swiftでは、URLRequestクラス以外にもAlamofireというライブラリが広く利用されています。このライブラリを利用すると、さらに柔軟で強力なHTTP通信が可能になります。

## 参考情報

- Appleの公式ドキュメンテーション- URLRequest: https://developer.apple.com/documentation/foundation/urlrequest
- AlamofireのGitHubページ: https://github.com/Alamofire/Alamofire