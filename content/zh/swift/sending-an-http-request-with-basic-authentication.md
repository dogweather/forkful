---
title:                "使用基本认证发送http请求"
html_title:           "Bash: 使用基本认证发送http请求"
simple_title:         "使用基本认证发送http请求"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 什麼以及為何?  | What & Why?

HTTP是網護瀏覽常用的協定。HTTP基本認證則將用戶名和密碼編碼，在每個HTTP請求中一起發送，用作身份驗證。這是實現客戶端與伺服端交換數據的重要工具。

## 怎麼做?  | How to:

```swift
import Foundation

let username = "yourUsername"
let password = "yourPassword"

let loginData = String(format: "%@:%@", username, password).data(using: String.Encoding.utf8)!
let base64LoginData = loginData.base64EncodedString()

// create the request
let url = URL(string: "https://your-api.com/login")!
var request = URLRequest(url: url)
request.httpMethod = "POST"
request.setValue("Basic \(base64LoginData)", forHTTPHeaderField: "Authorization")

//execute the request
URLSession.shared.dataTask(with: request) { (data, response, error) in
    // here do something with the returned data
}.resume()
```
當你執行上述代碼之後，將會看到這樣的結果:

```swift
https://your-api.com/login // Logs the URL
POST // Logs the HTTP method
Basic xxxx // Logs the authorization header
```
## 深度探索 | Deep Dive

在HTTP設計初期，基本認證就已經存在，被認為是HTTP認證機制中最直接、最簡單的一種方式。但是它的安全性並不高，所有資訊都以明文方式透過網路進行傳送，因此現在多常會與SSL/TLS等加密技術一起使用。另外，JWT和OAuth等也是更為先進的認證方式之一，针對特殊情況或讓開發者有更多的選擇。

同樣，Swift的URLRequest、HTTPURLResponse、URLSession等類別，就是這種認證机制在Swift語言中的實現方式，讓開發者可以方便地進行HTTP請求和認證。

## 參閱資料 | See Also
HTTP Basic Authentication - [Link](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Headers/Authorization)
NSURLSession Class - [Link](https://developer.apple.com/documentation/foundation/urlsession)
Swift Authentication with URLRequest - [Link](https://developer.apple.com/documentation/foundation/url_loading_system/basic_http_get_request)