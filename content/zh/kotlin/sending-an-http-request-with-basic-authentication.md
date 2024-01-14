---
title:                "Kotlin: 使用基本认证发送http请求"
simple_title:         "使用基本认证发送http请求"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

Mandarin:
## 为什么

如果你是一名Kotlin程序员，并且想要和后端服务器进行通信，那么发送HTTP请求是必不可少的。通过使用基本身份验证，你可以安全地将数据发送到服务器，并获得响应。在这篇博客文章中，我们将学习如何通过Kotlin发送带有基本身份验证的HTTP请求。

## 如何做

首先，我们需要导入Kotlin的相关库。在这个例子中，我们将使用`OkHttp`库。

```Kotlin
import okhttp3.*
```

然后，我们需要创建一个`OkHttpClient`对象，并使用`.newBuilder()`来添加基本身份验证。在这个例子中，我们将使用用户名为“username”和密码为“password”的凭据。

```Kotlin
val client = OkHttpClient.Builder()
    .authenticator(BasicAuthenticator("username", "password"))
    .build()
```

接下来，我们需要创建一个`Request`对象，并设置HTTP请求的方法、URL和请求体（如果有的话）。

```Kotlin
val request = Request.Builder()
    .method("POST", RequestBody.create(null, ""))
    .url("https://example.com/api")
    .build()
```

最后，我们通过使用`client`对象来发送请求，并将响应保存为一个`Response`对象。

```Kotlin
val response = client.newCall(request).execute()
```

现在，我们可以从`response`对象中获取响应体和其他相关信息。

```Kotlin
println(response.body()?.string())
println(response.code())
```

运行以上代码，你将会得到一个带有基本身份验证的HTTP请求的响应。如果身份验证成功，响应代码将会是200，同时你也可以看到从服务器返回的数据。

## 深入了解

基本身份验证是一种通过在请求头中包含Base64编码的用户名和密码来进行身份验证的方法。它在每次请求时都需要进行验证，因此如果你想要发送多个带有身份验证的请求，你可能需要将身份验证添加到每一个请求中。

除了基本身份验证，还有其他类型的身份验证，比如Bearer身份验证，它使用记号（token）来进行身份验证。如果你想要进一步学习有关身份验证的知识，你可以参考下面的链接。

## 参考链接

- [Kotlin官方网站](https://kotlinlang.org/)
- [OkHttp库文档](https://square.github.io/okhttp/)
- [HTTP身份验证介绍](https://www.lifewire.com/understanding-http-basics-and-how-server-authentication-works-816585)
- [不同类型的HTTP身份验证](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)