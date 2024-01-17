---
title:                "使用基本身份验证发送http请求"
html_title:           "Swift: 使用基本身份验证发送http请求"
simple_title:         "使用基本身份验证发送http请求"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# 什么 & 为什么？

使用基本认证发送HTTP请求是一种通过发送用户名和密码来验证身份的方法。程序员通常会这样做是为了确保他们的应用程序能够安全地与服务器进行通信，以及需求受限制的资源。

# 如何：

```Swift
let url = URL(string: "https://example.com")
var request = URLRequest(url: url!)
let loginString = "\(username):\(password)"
let loginData = loginString.data(using: .utf8)!
let base64LoginString = loginData.base64EncodedString()
let authString = "Basic \(base64LoginString)"
request.setValue(authString, forHTTPHeaderField: "Authorization")
let task = URLSession.shared.dataTask(with: request) { data, response, error in
  guard let data = data,
        let response = response as? HTTPURLResponse,
        error == nil else {                                              // 服务器 返回响应给request请求
    print("error", error ?? "Unknown error")                            // 打印错误信息
    return
  }
  guard (200 ... 299) ~= response.statusCode else {                      // 状态代号处理
    print("statusCode should be 2xx, but is \(response.statusCode)")   // 打印 处理
    print("response = \(response)")
    return
  }
  let responseString = String(data: data, encoding: .utf8)
  print("responseString = \(responseString)")
}
task.resume()
```

# 深入了解：

发送HTTP请求的基本认证方法已存在了很长时间，最初是为了在早期的网络通信中验证身份。现在仍然使用这种方法是因为它相对简单，能够满足大多数的安全需求。程序员也可以使用其他类型的身份验证方法，例如OAuth，具体取决于他们的需求和服务器的要求。在实现基本认证时，需要注意两点：首先是确保用户名和密码以正确的格式进行编码，其次是设置正确的Authorization header。

# 参考链接：

- [Sending HTTP Requests in Swift](https://developer.apple.com/documentation/foundation/url_loading_system/sending_an_http_request)
- [Basic Access Authentication](https://tools.ietf.org/html/rfc7617)