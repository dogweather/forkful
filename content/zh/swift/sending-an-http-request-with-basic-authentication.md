---
title:                "发送带基本身份验证的http请求"
html_title:           "Swift: 发送带基本身份验证的http请求"
simple_title:         "发送带基本身份验证的http请求"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

##为什么

如果你想要在你的Swift应用程序中与服务器进行交互，那么发送HTTP请求是必不可少的。但是有些服务器要求进行身份验证才能访问特定的资源，这就需要使用基本认证来发送HTTP请求。

##如何进行

首先，我们需要将URL和请求类型（GET，POST，DELETE等）存储在常量中。然后，我们需要创建一个URLSession对象，并使用该对象来创建一个URLRequest。接下来，在该请求中添加基本认证信息，这需要通过将用户名和密码转换为Base64编码来实现。最后，我们可以使用URLSession对象来发送请求，并处理响应数据。

```Swift
let url = URL(string: "https://example.com/resource")
let request = URLRequest(url: url!)
let session = URLSession.shared

// Add basic authentication to request
let username = "username"
let password = "password"
let loginString = "\(username):\(password)"
let loginData = loginString.data(using: .utf8)
let base64LoginString = loginData?.base64EncodedString()

request.addValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")

// Send the request and handle the response
let task = session.dataTask(with: request) { data, response, error in
    guard let data = data, error == nil else {
        print(error?.localizedDescription ?? "No data")
        return
    }
    if let response = response as? HTTPURLResponse {
        print(response.statusCode)
        print(String(data: data, encoding: .utf8)!)
    }
}

task.resume()
```

##深入探讨

基本认证要求在请求头中添加一个名为"Authorization"的字段，其值为"Basic"加上base64编码的用户名和密码。这样，服务器就可以使用这些信息来验证请求的发送者。当然，这种简单的认证方式并不安全，因为用户名和密码是以明文形式传输的。因此，我们应该尽可能地使用HTTPS来加密我们的请求和响应数据。

##另请参阅

- [使用Alamofire发送基本认证HTTP请求](https://www.swiftbysundell.com/articles/using-alamofire-to-interact-with-a-rest-api/)
- [官方文档：URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [官方文档：URLRequest](https://developer.apple.com/documentation/foundation/urlrequest)