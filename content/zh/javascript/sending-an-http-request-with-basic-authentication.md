---
title:                "使用基本认证发送http请求"
html_title:           "Bash: 使用基本认证发送http请求"
simple_title:         "使用基本认证发送http请求"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# 用基本身份验证发送HTTP请求的JavaScript编程

## 什么 & 为什么?

发送带有基本身份验证的HTTP请求是一种向服务器发送信息，同时通过用户名和密码验证自己身份的方法。程序员之所以这样做，是因为HTTPS请求可以从服务器获取携带特定数据的响应，而基本身份验证则保证了数据安全。

## 如何实现:

在Javascript中，你可以使用fetch来发送带有基本身份验证的请求。以下是一个例子：

```Javascript
const username = 'your-username';
const password = 'your-password';

fetch('https://your-api-url.com', {
  method: 'GET',
  headers: {
    'Authorization': 'Basic ' + btoa(username + ":" + password)
  }
})
.then(response => response.json())
.then(data => console.log(data))
.catch(error => console.error('Error:', error));
```
此代码首先创建了一个带有基本认证头的fetch请求，然后处理得到的json响应，最后用console.log打印数据，或者catch块处理错误。

## 深入了解

发送带有基本身份验证的HTTP请求的做法可以追溯到HTTP/1.0的早期。然而，该方法不应用于非加密通信，因为用户名和密码以明文形式发送，易遭监听。

如今，有许多替代方法可以用于身份验证，如OAuth和JWT。然而，基本身份验证的实现仍广泛使用，因为它简单、易于理解。

在发送带有基本身份验证的HTTP请求时，重要的是正确地构建'Authorization'头。其中的用户名和密码需要用`:`连接，然后用Base64进行编码。

## 参考资料

- MDN的fetch文档: [MDN Fetch](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- 基本认证详解: [HTTP Basic Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
- 替代的认证方法： [OAuth](https://oauth.net/) 和 [JWT](https://jwt.io/)