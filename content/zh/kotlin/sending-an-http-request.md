---
title:                "发送http请求"
html_title:           "C#: 发送http请求"
simple_title:         "发送http请求"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 什么和为什么?

发送HTTP请求就是从客户端向服务器发送请求，获取或提交数据。程序员需要发送HTTP请求以从网络服务中获取数据，或者将数据提交到网络服务。

## 如何操作:

下面使用Kotlin中的Ktor库来执行HTTP请求。

安装Ktor的HttpClient模块：

```Kotlin
dependencies {
    implementation("io.ktor:ktor-client-core:1.5.4")
    implementation("io.ktor:ktor-client-cio:1.5.4")
}
```

发送GET请求示例：

```Kotlin
import io.ktor.client.*
import io.ktor.client.request.*

suspend fun main() {
    val client = HttpClient()
    val response: String = client.get("https://jsonplaceholder.typicode.com/posts/1")

    println(response)
    client.close()
}
```

输出样例：

```text
{
  "userId": 1,
  "id": 1,
  "title": "sunt aut facere repellat provident occaecati excepturi optio reprehenderit",
  "body": "quia et suscipit\nsuscipit recusandae consequuntur expedita et cum\nreprehenderit molestiae ut ut quas totam\nnostrum rerum est autem sunt rem eveniet architecto"
}
```

## 深入探讨

HTTP请求的定义和实现始于1990年代初的Web发展。因此HTTP请求被视为Web通信的基础。

发送HTTP请求的替代方法有很多，例如使用Java的HttpClient或者第三方库，如OkHttp，Retrofit等。

发送HTTP的实现涉及到构建请求，硬编码URL，设置HTTP方法（GET，POST，PUT等），添加请求头，发送请求，并处理响应。在Ktor中，HttpClient构造函数创建一个客户端实例。然后，您可以使用get，post等函数设置HTTP方法和URL。最后，您将处理并打印从服务器获得的响应。

## 另请参阅

更多关于发送HTTP请求和Ktor库的信息，您可以参考以下资源：

1. 官方文档：[http://ktor.io/clients/http-client.html](http://ktor.io/clients/http-client.html)