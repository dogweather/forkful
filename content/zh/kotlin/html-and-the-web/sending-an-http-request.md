---
date: 2024-01-20 18:00:17.903600-07:00
description: "\u4EC0\u9EBC\u548C\u70BA\u4EC0\u9EBC\uFF1F \u53D1\u9001HTTP\u8BF7\u6C42\
  \u5C31\u662F\u5411\u4E00\u4E2A\u670D\u52A1\u5668\u53D1\u9001\u4E00\u6761\u6D88\u606F\
  \u5E76\u7B49\u5F85\u54CD\u5E94\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u4E3B\u8981\
  \u662F\u4E3A\u4E86\u4E0E\u8FDC\u7A0B\u670D\u52A1\u4EA4\u4E92\uFF0C\u83B7\u53D6\u6570\
  \u636E\u6216\u8005\u89E6\u53D1\u7F51\u7EDC\u4E0A\u7684\u884C\u4E3A\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:06.746428
model: gpt-4-1106-preview
summary: "\u4EC0\u9EBC\u548C\u70BA\u4EC0\u9EBC\uFF1F \u53D1\u9001HTTP\u8BF7\u6C42\u5C31\
  \u662F\u5411\u4E00\u4E2A\u670D\u52A1\u5668\u53D1\u9001\u4E00\u6761\u6D88\u606F\u5E76\
  \u7B49\u5F85\u54CD\u5E94\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u4E3B\u8981\u662F\
  \u4E3A\u4E86\u4E0E\u8FDC\u7A0B\u670D\u52A1\u4EA4\u4E92\uFF0C\u83B7\u53D6\u6570\u636E\
  \u6216\u8005\u89E6\u53D1\u7F51\u7EDC\u4E0A\u7684\u884C\u4E3A\u3002"
title: "\u53D1\u51FA HTTP \u8BF7\u6C42"
---

{{< edit_this_page >}}

## What & Why?
什麼和為什麼？ 发送HTTP请求就是向一个服务器发送一条消息并等待响应。程序员这么做主要是为了与远程服务交互，获取数据或者触发网络上的行为。

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
