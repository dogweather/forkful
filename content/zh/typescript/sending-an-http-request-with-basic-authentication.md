---
title:                "使用基本认证发送http请求"
html_title:           "TypeScript: 使用基本认证发送http请求"
simple_title:         "使用基本认证发送http请求"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

什么和为什么：

发送带有基本身份验证的 HTTP 请求是为了允许程序员在网络应用程序中进行用户认证。通过使用用户名和密码来验证身份，基本身份验证确保只有授权用户才能访问受保护的资源。这在保护隐私信息和防止未经授权的访问方面非常重要。

如何：

使用 TypeScript ，发送带有基本身份验证的 HTTP 请求非常简单。首先，您需要导入“http”模块，并使用“request”方法来创建一个请求对象。然后，将所要请求的 URL 和要发送的身份验证信息作为参数传递给该方法。最后，您可以使用“then”方法来处理响应并处理返回的数据。

```
import { request } from 'http';

const url = 'https://www.example.com/api/test';
const username = 'user123';
const password = 'pass456';

request({ url, auth: { username, password } })
.then(response => {
  // 处理响应
  console.log(response);
})
.catch(error => {
  // 处理错误
  console.log(error);
})
```

深入探讨：

基本身份验证是最早的身份验证机制之一，最初用于保护 Web 浏览器和服务器之间的通信。它使用 Base64 编码来加密用户的凭证，因此并不是最安全的身份验证方式。程序员也可以使用其他手段来实现身份验证，例如使用 OAuth 和 JSON Web Tokens (JWTs)。

欲了解更多关于基本身份验证的实现细节，请参考 RFC 2617。同时，您也可以查询有关更安全的身份验证方法的资料，例如基于令牌的身份验证。

相关资源：

- [RFC 2617](https://tools.ietf.org/html/rfc2617)
- [使用 TypeScript 发送 HTTP 请求](https://www.typescriptlang.org/docs/handbook/advanced-types.html#generic-object-types)
- [什么是基于令牌的身份验证？](https://jwt.io/introduction/)