---
title:                "使用基本认证发送 HTTP 请求"
date:                  2024-01-20T18:02:53.456488-07:00
model:                 gpt-4-1106-preview
simple_title:         "使用基本认证发送 HTTP 请求"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (什么以及为什么?)
我们经常需要让应用程序通过网络与服务器交互，发送HTTP请求是其中的基本方式。使用基本认证（basic authentication）可以快速简单地验证用户身份，确保数据的安全交换。

## How to (如何执行)
以下是使用Swift（最新版本）发送带有基本认证的HTTP请求的简单代码示例。

```Swift
import Foundation

// 准备URL和认证信息
if let url = URL(string: "https://your-api-endpoint.com/data") {
    var request = URLRequest(url: url)
    let username = "user"
    let password = "pass"
    let loginString = "\(username):\(password)"
    
    // 将认证信息转为base64
    if let loginData = loginString.data(using: .utf8) {
        let base64LoginString = loginData.base64EncodedString()
        
        // 在请求头里添加认证信息
        request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")
        
        // 执行请求
        URLSession.shared.dataTask(with: request) { (data, response, error) in
            // 确保没有发生错误
            if let error = error {
                print("请求发送错误: \(error)")
                return
            }
            
            // 确定我们得到了数据
            if let data = data, let dataString = String(data: data, encoding: .utf8) {
                print("服务器响应:\n\(dataString)")
            }
        }.resume()
    }
}
```

## Deep Dive (深入了解)
基本认证是HTTP协议中一种老旧但广泛支持的简单认证形式。它通过发送用户名和密码的Base64编码形式来验证用户身份。虽然基本认证很方便，但因为它不是特别安全（比如在非HTTPS连接中容易遭到中间人攻击），所以它通常不适用于敏感数据的传输。更安全的做法是使用令牌（token-based authentication）或OAuth等更复杂的认证机制。在Swift中实现基本认证主要是通过URLRequest对象进行的，它允许你设置请求的各种参数，包括HTTP头部。

## See Also (相关链接)
- [Swift Documentation](https://swift.org/documentation/)
- [Apple's URLSession Tutorial](https://developer.apple.com/documentation/foundation/urlsession)
- [HTTP Basic Authentication Scheme RFC](https://tools.ietf.org/html/rfc7617)
