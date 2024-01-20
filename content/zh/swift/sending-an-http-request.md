---
title:                "发送http请求"
html_title:           "C#: 发送http请求"
simple_title:         "发送http请求"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 什么是HTTP请求，为什么要发送它?

HTTP请求是在网络编程中发送到服务器的一种请求方式，开发者通常使用它来获取或者发送数据。

## 如何实现:
以下是在Swift中创建并发送HTTP请求的一个例子。首先，你创建了一个URL对象，然后创建了一个URLRequest对象。最后你创建了一个URLSession对象并调用了他的dataTask方法来发送请求:

```Swift
import Foundation

let url = URL(string: "https://www.example.com/")!
var request = URLRequest(url: url)

request.httpMethod = "GET"
let task = URLSession.shared.dataTask(with: request) { (data, response, error) in
    if let error = error {
        print("Error: \(error)")
    } else if let data = data {
        let str = String(data: data, encoding: .utf8)
        print("Received data:\n\(str ?? "")")
    }
}
task.resume()
```
输出:

Received data:
This is example data.

## 深度解析：
HTTP请求的历史可以追溯到1990年代早期的万维网。从那时起，开发者就开始使用它获取或发送网页和其他类型的数据。虽然有其他的替代方法，如WebSocket或HTTP/2，但HTTP请求仍然是最常用的方式。

在Swift和其他现代编程语言中，发送HTTP请求的实现细节主要依赖于具体的库和框架。大多数语言都有内建的库作为HTTP客户端，Swift也不例外。在上面的例子中，`dataTask(with:request)`函数由底层的URLSession对象处理，这是Apple的Foundation框架提供的。

## 更多参考资料：

- [Apple Swift Documentation](https://developer.apple.com/documentation/swift)
- [The Swift Programming Language Book by Apple](https://docs.swift.org/swift-book/)
- [HTTP: The Protocol Every Web Developer Must Know](https://code.tutsplus.com/tutorials/http-the-protocol-every-web-developer-must-know-part-1--net-31177)