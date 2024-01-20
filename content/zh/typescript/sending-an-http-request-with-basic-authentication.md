---
title:                "使用基本认证发送http请求"
html_title:           "Bash: 使用基本认证发送http请求"
simple_title:         "使用基本认证发送http请求"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 是什么？为什么？

HTTP基本认证发送请求是一种通过添加用户名称和密码到HTTP头来进行认证的方式。程序员会这样做为了实现服务和服务之间的认证，或者在不需要创建用户帐户的情况下实现系统的快速认证。

## 如何操作：

在TypeScript中，您可以使用axios库来发送具有HTTP基本认证的请求。以下是如何做到这一点的代码示例:

```TypeScript
import axios from 'axios';

const fetchData = async () => {
    const result = await axios({
      method: 'get',
      url: 'https://myapi.com',
      auth: {
        username: 'myUsername',
        password: 'myPassword'
      }
    });
    
    console.log(result.data);
}

fetchData();
```
运行上述代码后，您的输出将会是您的HTTP响应的数据。

## 深度研究

HTTP的基本认证生成于HTTP/1.0，并且至今仍普遍使用，主要是因为其简单易用性。虽然它并不如一些现代的替代方案如OAuth或Bearer tokens安全，但是在未公开或需要快速原型的api上还是十分常用。

在TypeScript对axios库的使用中，通过将auth对象添加到配置对象，实现基本认证。这将自动在每个请求的headers中添加适当的`Authorization`头，并使用Base64编码的“用户名:密码”格式。

## 参考

- Axios库文档：https://axios-http.com/docs/basic_features
- MDN关于HTTP基本访问认证：https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Authentication
- Overview of HTTP: https://zh.wikipedia.org/wiki/HTTP