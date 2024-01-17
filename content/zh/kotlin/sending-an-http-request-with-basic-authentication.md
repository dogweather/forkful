---
title:                "基本认证下发送http请求"
html_title:           "Kotlin: 基本认证下发送http请求"
simple_title:         "基本认证下发送http请求"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

什么 & 为什么？

HTTP请求是Web开发中常用的一种技术，它允许前端应用程序向服务器发送请求，以获取数据或执行某些操作。使用基本身份验证将用户名和密码包含在HTTP请求中可以提高安全性，确保只有授权用户才能访问数据。因此，程序员经常使用基本身份验证来保护和授权他们的应用程序。

如何：

下面是一个使用基本身份验证发送HTTP请求的Kotlin代码示例：

```
val username = "John"
val password = "123456"
val url = URL("http://example.com")
val connection = url.openConnection() as HttpURLConnection
connection.requestMethod = "GET"
val auth = "$username:$password"
val encodedAuth = Base64.getEncoder().encodeToString(auth.toByteArray())
connection.setRequestProperty("Authorization", "Basic $encodedAuth")
val responseCode = connection.responseCode
println("Response code: $responseCode")
```

运行以上代码，输出将是：`Response code: 200`，表示请求成功，服务器返回了预期的响应。
 
深入探讨：

基本身份验证是HTTP协议早期版本的一种身份验证机制，在互联网上广泛使用。然而，它存在一个弱点，即用户名和密码以纯文本形式发送，容易遭受黑客攻击。因此，现在已有更安全的身份验证机制，如OAuth。但是，基本身份验证仍然是一种简单且广泛支持的方式，可以在许多场景下使用。

相关链接：

- [Kotlin官方文档](https://kotlinlang.org/docs/reference/)
- [HTTP请求与Kotlin](https://www.tutorialkart.com/kotlin/kotlin-http-request-get-post-data/)
- [基本身份验证与OAuth：什么是更好的选择？](https://blog.cpanel.com/http-basic-auth-vs-oauth-which-is-the-better-choice/)