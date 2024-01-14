---
title:                "Javascript: 使用基本认证发送http请求"
simple_title:         "使用基本认证发送http请求"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 为什么
为什么发送带有基本身份验证的HTTP请求是重要的？基本身份验证是一个常用的安全措施，它允许用户通过提供用户名和密码来验证自己的身份。这样可以防止未经授权的用户访问敏感信息，保护网站的安全。通过发送带有基本身份验证的HTTP请求，可以确保只有授权用户才能够访问受保护的资源。

## 如何
要发送带有基本身份验证的HTTP请求，我们首先需要准备认证凭据。假设我们有一个用户名为“john”和密码为“123456”的用户，我们可以通过以下代码创建一个基本身份验证凭据：

```Javascript
const username = "john";
const password = "123456";
const credentials = btoa(`${username}:${password}`);
```

其中，`btoa()`函数用于将字符串编码为base64格式。接下来，我们需要构造HTTP请求，并将凭证添加到请求头中：

```Javascript
const url = "https://example.com/api/resource";
const options = {
    method: "GET",
    headers: {
        "Authorization": `Basic ${credentials}`
    }
};
fetch(url, options)
    .then(response => response.json())
    .then(data => console.log(data))
    .catch(err => console.error(err));
```

以上代码使用了`fetch()`函数来发送HTTP请求，并使用基本身份验证凭据作为请求头的一部分。如果凭据有效，则服务器将返回所请求的资源。

## 深入了解
当前的网络环境中，基本身份验证已不再被认为是足够安全的方法。因此，建议在发送HTTP请求时，使用更安全的身份验证方法，如OAuth。此外，在开发过程中，也应该注意处理凭证的安全存储和传输。

## 参考链接
- [MDN Web 文档：了解 HTTP 基本认证](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Authentication)
- [MDN Web 文档：使用 fetch](https://developer.mozilla.org/zh-CN/docs/Web/API/Fetch_API/Using_Fetch)
- [基本身份验证和使用 Fetch API 发送 HTTP 请求](https://www.freecodecamp.org/news/how-to-structure-a-blog-post-and-not-commit-to-the-first-idea/)