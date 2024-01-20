---
title:                "下载网页"
html_title:           "Arduino: 下载网页"
simple_title:         "下载网页"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
下载网页指的是从互联网上获取网站的HTML内容。程序员需要这样做来获取网页的数据或运行一些服务器侧的脚本。

## 如何做：
在Swift (当前版)中，你可以使用`URLSession`对象获取网页内容。以下是如何实现的代码片段和输出：
```Swift
import Foundation

if let url = URL(string: "https://neyourwebsite.com") {
    let task = URLSession.shared.dataTask(with: url) { (data, response, error) in
        if let data = data {
            print(String(data: data, encoding: .utf8)!)
        }
    }
    task.resume()
}
```
在上例中，我们创建一个URL对象，然后使用`URLSession`的`dataTask`方法去获取数据。拿到数据后，转成String格式并打印。

## 深入学习
在Swift出现之前，Objective-C 是 Apple 的主要开发语言。然而，Swift 在运行速度、安全性和易读性等方面许多特性都优于 Objective-C，这也是为何 Apple 选择推出 Swift。

然而除了 Swift，我们也有其他方式获取网页数据，例如使用WebSocket，HTTP/2等协议。WebSocket可提供全双工通信，适合需要“推送”数据的情况。HTTP/2是HTTP协议的最新版本，可提供更早的资源推送和服务器推送等特性。

Swift中下载网页数据利用的是URLSession框架。URLSession提供了一组API，你可以用它来处理HTTP／HTTPS的数据传输任务。

## 参见资料
- Apple官方文档 - URLSession: 
https://developer.apple.com/documentation/foundation/urlsession
- 了解更多HTTP/2: 
https://http2.github.io
- WebSocket详解: 
https://developer.mozilla.org/zh-CN/docs/Web/API/WebSocket