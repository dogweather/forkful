---
title:                "发送http请求"
html_title:           "Kotlin: 发送http请求"
simple_title:         "发送http请求"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

# 什么 & 为什么?

发送HTTP请求是指向服务器发送一个请求以获取特定信息的过程。程序员经常这样做是因为它可以帮助他们从远程服务器获取所需的数据。

# 如何进行:

```Kotlin
val url = URL("https://www.example.com") // 创建一个URL对象
val connection = url.openConnection() as HttpURLConnection // 创建一个HTTP连接
val responseCode = connection.responseCode // 获取服务器返回的响应码
println("响应码: $responseCode")
```

```Kotlin
// 创建一个新的HTTP连接
val connection = URL("https://www.example.com").openConnection() as HttpURLConnection
// 指定请求方法和超时时间
connection.requestMethod = "GET"
connection.connectTimeout = 5000
// 设置请求头
connection.setRequestProperty("Content-Type", "application/json")
// 获取服务器返回的响应内容
val response = connection.inputStream.bufferedReader().readText()
println(response)
```

输出: 
```
响应码: 200 
{
  "message" : "Hello World!"
}
```

# 深入探讨:

1. 历史背景: HTTP是一种用于客户端和服务器之间传输数据的通信协议。在互联网普及之后，它成为了最主流的协议。
2. 其他可能的选项: 此外，还有其他库和框架可用于发送HTTP请求，如OkHTTP和Retrofit。
3. 实现细节: 在Kotlin中发送HTTP请求可以通过Java的URLConnection类实现，也可以使用Ktor库来简化操作。

# 查看相关资源:

- Kotlin官方文档: https://kotlinlang.org/docs/reference/
- OkHTTP库文档: https://square.github.io/okhttp/
- Retrofit库文档: https://square.github.io/retrofit/
- Ktor库文档: https://ktor.io/