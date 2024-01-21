---
title:                "发出 HTTP 请求"
date:                  2024-01-20T18:00:30.701796-07:00
model:                 gpt-4-1106-preview
simple_title:         "发出 HTTP 请求"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)

HTTP请求让应用可以和服务器交流数据。程序员用它来获取信息、发送数据更新或者进行远程服务调用。

## How to: (怎么做：)

Swift发送HTTP请求很简单，用`URLSession`来完成基本的GET请求：

```Swift
import Foundation

let url = URL(string: "https://api.example.com/data")!
var request = URLRequest(url: url)
request.httpMethod = "GET"

let task = URLSession.shared.dataTask(with: request) { data, response, error in
    if let error = error {
        print("请求出错：\(error)")
    } else if let data = data, let dataString = String(data: data, encoding: .utf8) {
        print("响应数据：\(dataString)")
    }
}
task.resume()
```

简单的输出示例：

```
响应数据：{"name":"张三","age":30}
```

## Deep Dive (深潜) 

HTTP请求是Web发展关键技术。最初，HTTP是用于HTML页面获取，现在，它在RESTful API中占主要地位，这使得各种客户端技术如Swift都能访问服务数据。还有其他方案：比如GraphQL，它可以令请求更精确、高效。实现HTTP请求时，Swift使用`URLSession`API，它提供了同步和异步请求处理能力，并可进行请求定制。

## See Also (另请参阅)

- Apple的`URLSession`文档: [URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- 更多RESTful API信息: [RESTful API](https://restfulapi.net/)
- GraphQL 官方网站: [GraphQL](https://graphql.org/)

记得，练习和实验是掌握HTTP请求的关键。别忘了查看官方文档以获取最新和最深入的信息。