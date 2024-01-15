---
title:                "使用基本身份验证发送http请求"
html_title:           "Kotlin: 使用基本身份验证发送http请求"
simple_title:         "使用基本身份验证发送http请求"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 为什么要发送带基本身份验证的HTTP请求？

在网络开发中，有时候我们需要通过HTTP请求来与服务器进行通信，但是为了保证安全性，我们需要使用身份验证来确认请求的发送者身份。基本身份验证是一种简单的验证方式，可以有效地保护服务器免受未经授权的请求。

## 如何发送带基本身份验证的HTTP请求

```Kotlin
val url = URL("https://example.com/api")
val connection = url.openConnection() as HttpURLConnection
connection.requestMethod = "GET"
connection.setRequestProperty("Authorization", "Basic " + Base64.getEncoder().encodeToString("username:password".toByteArray()))
val responseCode = connection.responseCode
val responseMessage = connection.responseMessage
println("Response Code: $responseCode")
println("Response Message: $responseMessage")
```
执行以上代码，可以获取到服务器响应的状态码和消息，用来确认请求是否成功发送和身份验证是否通过。

## 深入了解发送带基本身份验证的HTTP请求

在上述示例中，我们使用了Base64编码来对用户名和密码进行加密，并将其放在HTTP请求头中的"Authorization"字段中。服务器在接收到请求后，会将该字段进行解码，并与存储的用户名和密码进行比对，从而判断请求是否允许通过。

另外，如果身份验证失败，服务器会返回401未授权的响应码，这也是我们在开发过程中需要注意的地方。

## 参考链接

- [Kotlin官方网站](https://kotlinlang.org/)
- [HTTP基本身份验证](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Authentication)
- [Kotlin中的Base64编码](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/base64-encoding.html)

# 查看更多

如果想了解更多关于Kotlin的知识，请查看以下链接：

- [Kotlin中文社区](https://www.kotlincn.net/)
- [Kotlin深入浅出](https://www.kotlincn.net/docs/reference/)
- [Kotlin入门视频教程](https://www.youtube.com/watch?v=_QVl0zRqCk8)