---
date: 2024-01-20 18:00:17.903600-07:00
description: "How to: \u5728Kotlin\u4E2D\u53D1\u9001HTTP\u8BF7\u6C42\u53EF\u4EE5\u7528\
  \u51E0\u79CD\u5E93\uFF0C\u8FD9\u91CC\u6211\u4EEC\u7528Ktor\u7684\u5BA2\u6237\u7AEF\
  \u6A21\u5757\u3002\u786E\u4FDD\u5728\u9879\u76EE\u4E2D\u5F15\u5165Ktor\u4F9D\u8D56\
  \u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.034250-06:00'
model: gpt-4-1106-preview
summary: "\u5728Kotlin\u4E2D\u53D1\u9001HTTP\u8BF7\u6C42\u53EF\u4EE5\u7528\u51E0\u79CD\
  \u5E93\uFF0C\u8FD9\u91CC\u6211\u4EEC\u7528Ktor\u7684\u5BA2\u6237\u7AEF\u6A21\u5757\
  \u3002\u786E\u4FDD\u5728\u9879\u76EE\u4E2D\u5F15\u5165Ktor\u4F9D\u8D56\u3002"
title: "\u53D1\u51FA HTTP \u8BF7\u6C42"
weight: 44
---

## How to:
在Kotlin中发送HTTP请求可以用几种库，这里我们用Ktor的客户端模块。确保在项目中引入Ktor依赖。

```kotlin
import io.ktor.client.*
import io.ktor.client.engine.cio.*
import io.ktor.client.request.*
import io.ktor.client.statement.*

suspend fun main() {
    val client = HttpClient(CIO)
    val response: HttpResponse = client.get("https://httpbin.org/get")
    println(response.status)
    println(response.readText())
    client.close()
}

// Sample Output
// OK
// {
//   "args": {},
//   "headers": {
//     ...
//   },
//   "origin": "123.45.67.89",
//   "url": "https://httpbin.org/get"
// }
```

记得，Kotlin的协程需要在`suspend`函数中运行。

## Deep Dive:
HTTP请求的概念随着网络在90年代初期的发展而普及。Kotlin社区推荐了几个HTTP客户端库，如Ktor和Fuel。Ktor以其异步性质和易用性著名，它由Kotlin团队官方支持，完全用Kotlin编写。发送HTTP请求时，你能设置请求如方法(GET、POST等)、头部、体部和超时。了解这些对于使用API和构建网络应用程序很重要。

## See Also:
- Ktor文档: [https://ktor.io/docs/welcome.html](https://ktor.io/docs/welcome.html)
- Kotlin官方文档: [https://kotlinlang.org/docs/home.html](https://kotlinlang.org/docs/home.html)
- HTTP协议入门: [https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Overview](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Overview)
