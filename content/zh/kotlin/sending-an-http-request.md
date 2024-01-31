---
title:                "发出 HTTP 请求"
date:                  2024-01-20T18:00:17.903600-07:00
model:                 gpt-4-1106-preview
simple_title:         "发出 HTTP 请求"

category:             "Kotlin"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/sending-an-http-request.md"
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
